#include "arg-counter.h"
#include "vctrs.h"
#include "decl/arg-counter-decl.h"


static
struct counters* new_counters(r_obj* names,
                              struct vctrs_arg* p_curr_arg,
                              struct vctrs_arg* p_parent_arg,
                              struct counters* prev_box_counters,
                              struct counters* next_box_counters) {
  // This protects `shelter` and `names`. We leave space for
  // protecting `prev_box_counters` and `next_box_counters` later on.
  r_obj* shelter = KEEP(r_alloc_list(COUNTERS_SHELTER_N));

  r_obj* data_shelter = r_alloc_raw(sizeof(struct counters));
  r_list_poke(shelter, COUNTERS_SHELTER_data, data_shelter);

  // `names` might be from a splice box whose reduction has already
  // finished. We protect those from upstack.
  r_list_poke(shelter, COUNTERS_SHELTER_names, names);

  struct counters* p_counters = r_raw_begin(data_shelter);
  p_counters->shelter = shelter;

  p_counters->curr = 0;
  p_counters->next = 0;

  p_counters->names = names;
  p_counters->names_curr = 0;
  p_counters->names_next = 0;

  p_counters->curr_counter_arg_data = new_counter_arg_data(p_parent_arg,
                                                           &p_counters->curr,
                                                           &p_counters->names,
                                                           &p_counters->names_curr);
  p_counters->next_counter_arg_data = new_counter_arg_data(p_parent_arg,
                                                           &p_counters->next,
                                                           &p_counters->names,
                                                           &p_counters->names_next);

  p_counters->curr_counter = new_counter_arg(p_parent_arg, (void*) &p_counters->curr_counter_arg_data);
  p_counters->next_counter = new_counter_arg(p_parent_arg, (void*) &p_counters->next_counter_arg_data);

  p_counters->curr_arg = p_curr_arg;
  p_counters->next_arg = (struct vctrs_arg*) &p_counters->next_counter;

  p_counters->prev_box_counters = prev_box_counters;
  p_counters->next_box_counters = next_box_counters;

  FREE(1);
  return p_counters;
}

static
void init_next_box_counters(struct vctrs_arg* p_parent_arg,
                            struct counters* p_counters,
                            r_obj* names) {
  p_counters->prev_box_counters = p_counters->next_box_counters;
  r_list_poke(p_counters->shelter,
              COUNTERS_SHELTER_prev,
              r_list_get(p_counters->shelter, COUNTERS_SHELTER_next));

  struct counters* p_next = new_counters(names,
                                         p_counters->curr_arg,
                                         p_parent_arg,
                                         NULL,
                                         NULL);
  r_list_poke(p_counters->shelter, COUNTERS_SHELTER_next, p_next->shelter);
  p_counters->next_box_counters = p_next;

  p_next->next = p_counters->next;
}

static
void counters_inc(struct counters* counters) {
  ++(counters->next);
  ++(counters->names_next);
}

/**
 *  Swap counters so that the `next` counter (the one being increased
 *  on iteration and representing the new input in the reduction)
 *  becomes the current counter (the one representing the result so
 *  far of the reduction).
 */
void counters_shift(struct counters* p_counters) {
  // Swap the counters data
  SWAP(struct vctrs_arg, p_counters->curr_counter, p_counters->next_counter);
  SWAP(r_ssize*, p_counters->curr_counter_arg_data.i, p_counters->next_counter_arg_data.i);
  SWAP(r_ssize*, p_counters->curr_counter_arg_data.names_i, p_counters->next_counter_arg_data.names_i);

  // Update the handles to `vctrs_arg`
  p_counters->curr_arg = (struct vctrs_arg*) &p_counters->curr_counter;
  p_counters->next_arg = (struct vctrs_arg*) &p_counters->next_counter;

  // Update the current index
  p_counters->curr = p_counters->next;
}


// Reduce `impl` with argument counters

r_obj* reduce(r_obj* current,
              struct vctrs_arg* p_current_arg,
              struct vctrs_arg* p_parent_arg,
              r_obj* rest,
              r_obj* (*impl)(r_obj* current, r_obj* next, struct counters* counters, void* data),
              void* data) {
  // Store the box counters here as they might outlive their frame
  struct counters next_box_counters;
  struct counters prev_box_counters;

  struct counters* p_counters = new_counters(r_names(rest),
                                             p_current_arg,
                                             p_parent_arg,
                                             &prev_box_counters,
                                             &next_box_counters);
  KEEP(p_counters->shelter);

  r_obj* out = reduce_impl(current,
                           rest,
                           p_parent_arg,
                           p_counters,
                           false,
                           impl,
                           data);

  FREE(1);
  return out;
}

static
r_obj* reduce_impl(r_obj* current,
                   r_obj* rest,
                   struct vctrs_arg* p_parent_arg,
                   struct counters* counters,
                   bool spliced,
                   r_obj* (*impl)(r_obj* current,
                                  r_obj* next,
                                  struct counters* counters,
                                  void* data),
                   void* data) {
  r_ssize n = r_length(rest);

  for (r_ssize i = 0; i < n; ++i, counters_inc(counters)) {
    KEEP(current);

    r_obj* next = r_list_get(rest, i);

    // Don't call `rlang_is_splice_box()` if we're already looking at a
    // spliced list because it's expensive
    if (spliced || !rlang_is_splice_box(next)) {
      current = impl(current, next, counters, data);
    } else {
      next = KEEP(rlang_unbox(next));
      current = reduce_splice_box(current,
                                  next,
                                  p_parent_arg,
                                  counters,
                                  impl,
                                  data);
      FREE(1);
    }

    FREE(1);
  }

  return current;
}

static
r_obj* reduce_splice_box(r_obj* current,
                         r_obj* rest,
                         struct vctrs_arg* p_parent_arg,
                         struct counters* counters,
                         r_obj* (*impl)(r_obj* current,
                                        r_obj* rest,
                                        struct counters* counters,
                                        void* data),
                         void* data) {
  init_next_box_counters(p_parent_arg, counters, r_names(rest));
  struct counters* box_counters = counters->next_box_counters;

  current = reduce_impl(current,
                        rest,
                        p_parent_arg,
                        box_counters,
                        true,
                        impl,
                        data);

  counters->curr_arg = box_counters->curr_arg;
  counters->next = box_counters->next;

  return current;
}
