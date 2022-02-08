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

  counters->curr_counter_data = new_counter_arg_data(&counters->curr, &counters->names, &counters->names_curr);
  counters->next_counter_data = new_counter_arg_data(&counters->next, &counters->names, &counters->names_next);

  counters->curr_counter = new_counter_arg(NULL, (void*) &counters->curr_counter_data);
  counters->next_counter = new_counter_arg(NULL, (void*) &counters->next_counter_data);

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
#define PROTECT_COUNTERS(counters, nprot) do {                                \
    PROTECT_WITH_INDEX((counters)->names, &(counters)->names_pi);             \
    PROTECT_WITH_INDEX(R_NilValue, &(counters)->prev_box_counters->names_pi); \
    PROTECT_WITH_INDEX(R_NilValue, &(counters)->next_box_counters->names_pi); \
    *nprot += 3;                                                              \
  } while(0)


void counters_inc(struct counters* counters) {
  ++(counters->next);
  ++(counters->names_next);
}

/**
 *  Swap counters so that the `next` counter (the one being increased
 *  on iteration and representing the new input in the reduction)
 *  becomes the current counter (the one representing the result so
 *  far of the reduction).
 *
 * [[ include("arg-counter.h") ]]
 */
void counters_shift(struct counters* counters) {
  // Swap the counters data
  SWAP(void*, counters->curr_counter.data, counters->next_counter.data);
  SWAP(R_len_t*, counters->curr_counter_data.i, counters->next_counter_data.i);
  SWAP(R_len_t*, counters->curr_counter_data.names_i, counters->next_counter_data.names_i);

  // Update the handles to `vctrs_arg`
  counters->curr_arg = (struct vctrs_arg*) &counters->curr_counter;
  counters->next_arg = (struct vctrs_arg*) &counters->next_counter;

  // Update the current index
  counters->curr = counters->next;
}


// Reduce `impl` with argument counters

SEXP reduce_impl(SEXP current,
                 SEXP rest,
                 struct counters* counters,
                 bool spliced,
                 SEXP (*impl)(SEXP current, SEXP next, struct counters* counters, void* data),
                 void* data);

SEXP reduce_splice_box(SEXP current,
                       SEXP rest,
                       struct counters* counters,
                       SEXP (*impl)(SEXP current, SEXP next, struct counters* counters, void* data),
                       void* data);

// [[ include("arg-counter.h") ]]
SEXP reduce(SEXP current, struct vctrs_arg* current_arg,
            SEXP rest,
            SEXP (*impl)(SEXP current, SEXP next, struct counters* counters, void* data),
            void* data) {
  // Store the box counters here as they might outlive their frame
  struct counters next_box_counters;
  struct counters prev_box_counters;

  struct counters counters;
  init_counters(&counters,
                r_names(rest),
                current_arg,
                &prev_box_counters,
                &next_box_counters);
  int nprot = 0;
  PROTECT_COUNTERS(&counters, &nprot);

  SEXP out = reduce_impl(current, rest, &counters, false, impl, data);

  UNPROTECT(nprot);
  return out;
}

SEXP reduce_impl(SEXP current,
                 SEXP rest,
                 struct counters* counters,
                 bool spliced,
                 SEXP (*impl)(SEXP current, SEXP next, struct counters* counters, void* data),
                 void* data) {
  R_len_t n = Rf_length(rest);

  for (R_len_t i = 0; i < n; ++i, counters_inc(counters)) {
    PROTECT(current);

    SEXP next = VECTOR_ELT(rest, i);

    // Don't call `rlang_is_splice_box()` if we're already looking at a
    // spliced list because it's expensive
    if (spliced || !rlang_is_splice_box(next)) {
      current = impl(current, next, counters, data);
    } else {
      next = PROTECT(rlang_unbox(next));
      current = reduce_splice_box(current, next, counters, impl, data);
      UNPROTECT(1);
    }

    UNPROTECT(1);
  }

  return current;
}

SEXP reduce_splice_box(SEXP current, SEXP rest, struct counters* counters,
                       SEXP (*impl)(SEXP current, SEXP rest, struct counters* counters, void* data),
                       void* data) {
  init_next_box_counters(counters, r_names(rest));
  struct counters* box_counters = counters->next_box_counters;

  current = reduce_impl(current, rest, box_counters, true, impl, data);

  counters->curr_arg = box_counters->curr_arg;
  counters->next = box_counters->next;

  return current;
}
