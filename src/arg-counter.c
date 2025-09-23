#include "arg-counter.h"
#include "vctrs.h"
#include "decl/arg-counter-decl.h"


static
struct counters* new_counters(r_obj* names,
                              struct vctrs_arg* p_curr_arg,
                              struct vctrs_arg* p_parent_arg) {
  // This protects `shelter` and `names`
  r_obj* shelter = KEEP(r_alloc_list(COUNTERS_SHELTER_N));

  r_obj* data_shelter = r_alloc_raw(sizeof(struct counters));
  r_list_poke(shelter, COUNTERS_SHELTER_data, data_shelter);

  // `names` probably don't need to be protected, but we do so to be safe
  // (We used to use splice boxes, and `names` needed to be protected then,
  // but we no longer do as of #1578)
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

  FREE(1);
  return p_counters;
}

static inline
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
  struct counters* p_counters = new_counters(r_names(rest),
                                             p_current_arg,
                                             p_parent_arg);
  KEEP(p_counters->shelter);

  r_obj* out = reduce_impl(current,
                           rest,
                           p_parent_arg,
                           p_counters,
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
                   r_obj* (*impl)(r_obj* current,
                                  r_obj* next,
                                  struct counters* counters,
                                  void* data),
                   void* data) {
  r_ssize n = r_length(rest);
  r_obj* const* v_rest = r_list_cbegin(rest);

  r_keep_loc current_pi;
  KEEP_HERE(current, &current_pi);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* next = v_rest[i];
    current = impl(current, next, counters, data);
    KEEP_AT(current, current_pi);
    counters_inc(counters);
  }

  FREE(1);
  return current;
}
