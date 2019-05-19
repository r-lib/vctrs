#include "vctrs.h"
#include "utils.h"
#include "arg.h"
#include "arg-counter.h"


void init_counters(struct counters* counters,
                   SEXP names,
                   struct vctrs_arg* curr_arg,
                   struct counters* prev_box_counters,
                   struct counters* next_box_counters) {
  counters->curr = 0;
  counters->next = 0;

  counters->names = names;
  counters->names_curr = 0;
  counters->names_next = 0;

  counters->curr_counter = new_counter_arg(NULL, &counters->curr, &counters->names, &counters->names_curr);
  counters->next_counter = new_counter_arg(NULL, &counters->next, &counters->names, &counters->names_next);

  counters->curr_arg = curr_arg;
  counters->next_arg = (struct vctrs_arg*) &counters->next_counter;

  counters->prev_box_counters = prev_box_counters;
  counters->next_box_counters = next_box_counters;
}

void init_next_box_counters(struct counters* counters, SEXP names) {
  SWAP(struct counters*, counters->prev_box_counters, counters->next_box_counters);
  struct counters* next = counters->next_box_counters;

  REPROTECT(names, next->names_pi);

  init_counters(next, names, counters->curr_arg, NULL, NULL);
  next->next = counters->next;
}

// Stack-based protection, should be called after `init_counters()`
int PROTECT_COUNTERS(struct counters* counters) {
  PROTECT_WITH_INDEX(counters->names, &counters->names_pi);
  PROTECT_WITH_INDEX(R_NilValue, &counters->prev_box_counters->names_pi);
  PROTECT_WITH_INDEX(R_NilValue, &counters->next_box_counters->names_pi);
  return 3;
}


void counters_inc(struct counters* counters) {
  ++(counters->next);
  ++(counters->names_next);
}

// Swap counters so that the `next` counter (the one being increased
// on iteration) becomes the current counter (the one queried when
// there is an error)
void counters_swap(struct counters* counters) {
  // Swap the counters data
  SWAP(struct vctrs_arg_counter, counters->curr_counter, counters->next_counter);
  SWAP(R_len_t*, counters->curr_counter.i, counters->next_counter.i);
  SWAP(R_len_t*, counters->curr_counter.names_i, counters->next_counter.names_i);

  // Update the handles to `vctrs_arg`
  counters->curr_arg = (struct vctrs_arg*) &counters->curr_counter;
  counters->next_arg = (struct vctrs_arg*) &counters->next_counter;

  // Update the current index
  counters->curr = counters->next;
}

