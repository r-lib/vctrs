#ifndef VCTRS_SLICE_ASSIGN_H
#define VCTRS_SLICE_ASSIGN_H

#include "vctrs-core.h"
#include "ownership.h"

/**
 * Optional `value` slicing
 *
 * - For `ASSIGNMENT_SLICE_VALUE_no`, assignment is treated the standard `[<-`
 *   way, i.e. `x[i] <- value`.
 *
 *   `value` must be size 1 or the same size as `i` after `i` has been converted
 *   to a positive integer location vector with `vec_as_location()` (but note
 *   that we don't actually make that conversion in the
 *   `VCTRS_INDEX_STYLE_condition` case, we just count the number of `TRUE` and
 *   `NA` values to perform the size check between `i` and `value`).
 *
 * - For `ASSIGNMENT_SLICE_VALUE_yes`, assignment proceeds as an optimized form
 *   of: `x[i] <- value[i]`. Internally, we avoid actually materializing the
 *   slice of `value`.
 *
 *   `value` must be size 1 or the same size as `x`.
 */
enum assignment_slice_value {
  ASSIGNMENT_SLICE_VALUE_no,
  ASSIGNMENT_SLICE_VALUE_yes
};

struct vec_assign_opts {
  bool assign_names;
  bool ignore_outer_names;
  enum vctrs_ownership ownership;
  enum assignment_slice_value slice_value;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
  struct r_lazy call;
};

struct vec_proxy_assign_opts {
  bool assign_names;
  bool ignore_outer_names;
  enum vctrs_ownership ownership;
  enum assignment_slice_value slice_value;
  enum vctrs_index_style index_style;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
  struct r_lazy call;
  // Whether the `proxy` was proxied recursively or not
  bool recursively_proxied;
};

r_obj* vec_assign_opts(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  const struct vec_assign_opts* p_opts
);

r_obj* vec_proxy_assign_opts(
  r_obj* proxy,
  r_obj* index,
  r_obj* value,
  const struct vec_proxy_assign_opts* p_opts
);

r_obj* chr_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

r_obj* list_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

r_obj* df_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  const struct vec_proxy_assign_opts* p_opts
);

r_obj* vec_assign_shaped(
  r_obj* proxy,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

bool is_condition_index(r_obj* index, r_ssize size);

void check_condition_index(
  r_obj* x,
  r_ssize size,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
);

void list_check_all_condition_indices(
  r_obj* xs,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

void check_recyclable_against_index(
  r_obj* value,
  r_obj* index,
  r_ssize size,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style,
  struct vctrs_arg* p_value_arg,
  struct r_lazy call
);

// Exposed for `slice-assign-array.c`
void check_assign_sizes(
  r_obj* x,
  r_obj* index,
  r_ssize value_size,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

// Exposed for `slice-assign-array.c`
static inline
bool should_slice_value(enum assignment_slice_value slice_value) {
  return slice_value == ASSIGNMENT_SLICE_VALUE_yes;
}

#endif
