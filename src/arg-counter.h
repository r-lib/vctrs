#ifndef VCTRS_ARG_COUNTER_H
#define VCTRS_ARG_COUNTER_H

#include "vctrs-core.h"

struct counters {
 /* public: */

  // Argument tags for the current value of the reduction (the result
  // so far) and the next value. These handles typically point to the
  // local counter args, but might also point to external arg objects
  // like the initial current arg, or a splice box counter arg.
  struct vctrs_arg* curr_arg;
  struct vctrs_arg* next_arg;

 /* private: */

  // Global counters
  R_len_t curr;
  R_len_t next;

  SEXP names;
  R_len_t names_curr;
  R_len_t names_next;

  // `names` might be from a splice box whose reduction has already
  // finished. We protect those from up high.
  PROTECT_INDEX names_pi;

  // Local counters for splice boxes. Since the tags are generated
  // lazily, we need two counter states to handle the
  // `vec_c(!!!list(foo = 1), !!!list(bar = 2))` case.
  struct counters* next_box_counters;
  struct counters* prev_box_counters;

  // Actual counter args are stored here
  struct arg_data_counter curr_counter_data;
  struct arg_data_counter next_counter_data;
  struct vctrs_arg curr_counter;
  struct vctrs_arg next_counter;
};

/**
 * Swap the argument tags of the reduction
 *
 * There are two counters used for generating argument tags when an
 * error occur during a reduction. The first represent the result so
 * far, and the second the next input. Call `counters_shift()` to set
 * the counter of the next input as current counter, and start
 * iterating with a new counter for the next input.
 */
void counters_shift(struct counters* counters);

SEXP reduce(SEXP current, struct vctrs_arg* current_arg,
            SEXP rest,
            SEXP (*impl)(SEXP current, SEXP next, struct counters* counters, void* data),
            void* data);


#endif
