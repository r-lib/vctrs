#include "vctrs.h"
#include "decl/arg-counter-decl.h"


struct counters_data {
  struct arg_data_counter curr_counter_data;
  struct arg_data_counter next_counter_data;
};

static
void init_counters(struct counters* counters,
                   r_obj* names,
                   struct vctrs_arg* p_curr_arg,
                   struct vctrs_arg* p_parent_arg,
                   struct counters* prev_box_counters,
                   struct counters* next_box_counters) {
  counters->shelter = KEEP(r_alloc_raw(sizeof(struct counters_data)));
  struct counters_data* p_data = r_raw_begin(counters->shelter);
  counters->p_data = p_data;

  counters->curr = 0;
  counters->next = 0;

  counters->names = names;
  counters->names_curr = 0;
  counters->names_next = 0;

  p_data->curr_counter_data = new_counter_arg_data(p_parent_arg,
                                                   &counters->curr,
                                                   &counters->names,
                                                   &counters->names_curr);
  p_data->next_counter_data = new_counter_arg_data(p_parent_arg,
                                                   &counters->next,
                                                   &counters->names,
                                                   &counters->names_next);

  counters->curr_counter = new_counter_arg(p_parent_arg, (void*) &p_data->curr_counter_data);
  counters->next_counter = new_counter_arg(p_parent_arg, (void*) &p_data->next_counter_data);

  counters->curr_arg = p_curr_arg;
  counters->next_arg = (struct vctrs_arg*) &counters->next_counter;

  counters->prev_box_counters = prev_box_counters;
  counters->next_box_counters = next_box_counters;

  FREE(1);
}

static
void init_next_box_counters(struct vctrs_arg* p_parent_arg,
                            struct counters* counters,
                            r_obj* names) {
  SWAP(struct counters*, counters->prev_box_counters, counters->next_box_counters);
  struct counters* next = counters->next_box_counters;

  KEEP_AT(names, next->names_pi);

  init_counters(next,
                names,
                counters->curr_arg,
                p_parent_arg,
                NULL,
                NULL);
  next->next = counters->next;
}

// Stack-based protection, should be called after `init_counters()`
#define PROTECT_COUNTERS(counters, nprot) do {                          \
    KEEP((counters)->shelter);                                          \
    KEEP_HERE((counters)->names, &(counters)->names_pi);                \
    KEEP_HERE(R_NilValue, &(counters)->prev_box_counters->names_pi);    \
    KEEP_HERE(R_NilValue, &(counters)->next_box_counters->names_pi);    \
    *nprot += 4;                                                        \
  } while(0)


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
void counters_shift(struct counters* counters) {
  struct counters_data* p_data = counters->p_data;

  // Swap the counters data
  SWAP(void*, counters->curr_counter.data, counters->next_counter.data);
  SWAP(r_ssize*, p_data->curr_counter_data.i, p_data->next_counter_data.i);
  SWAP(r_ssize*, p_data->curr_counter_data.names_i, p_data->next_counter_data.names_i);

  // Update the handles to `vctrs_arg`
  counters->curr_arg = (struct vctrs_arg*) &counters->curr_counter;
  counters->next_arg = (struct vctrs_arg*) &counters->next_counter;

  // Update the current index
  counters->curr = counters->next;
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

  struct counters counters;
  init_counters(&counters,
                r_names(rest),
                p_current_arg,
                p_parent_arg,
                &prev_box_counters,
                &next_box_counters);
  int nprot = 0;
  PROTECT_COUNTERS(&counters, &nprot);

  r_obj* out = reduce_impl(current,
                           rest,
                           p_parent_arg,
                           &counters,
                           false,
                           impl,
                           data);

  FREE(nprot);
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
  KEEP(counters->shelter);

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

  FREE(1);
  return current;
}
