#ifndef VCTRS_ARG_COUNTER_H
#define VCTRS_ARG_COUNTER_H


struct counters {
  // Global counters
  R_len_t curr;
  R_len_t next;

  SEXP names;
  R_len_t names_curr;
  R_len_t names_next;

  // `names` might be from a splice box whose reduction has already
  // finished. We protect those from up high.
  PROTECT_INDEX names_pi;

  // Local counters for splice boxes. We need two of those to handle
  // the `vec_c(!!!list(foo = 1), !!!list(bar = 2))` case.
  struct counters* next_box_counters;
  struct counters* prev_box_counters;

  // Actual counter args are stored here
  struct vctrs_arg_counter curr_counter;
  struct vctrs_arg_counter next_counter;

  // Polymorphic `vctrs_arg` handles. They typically point to the
  // local counter args, but might also point to external arg objects
  // like a `.ptype` arg, or a splice box counter arg.
  struct vctrs_arg* curr_arg;
  struct vctrs_arg* next_arg;
};


void init_counters(struct counters* counters,
                   SEXP names,
                   struct vctrs_arg* curr_arg,
                   struct counters* prev_box_counters,
                   struct counters* next_box_counters);

void init_next_box_counters(struct counters* counters, SEXP names);

// Stack-based protection, should be called after `init_counters()`
int PROTECT_COUNTERS(struct counters* counters);

void counters_inc(struct counters* counters);
void counters_swap(struct counters* counters);


#endif
