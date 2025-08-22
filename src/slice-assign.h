#ifndef VCTRS_SLICE_ASSIGN_H
#define VCTRS_SLICE_ASSIGN_H

#include "vctrs-core.h"
#include "ownership.h"

/**
 * Assignment style
 *
 * - Location indices can be integer, character, or logical, but are ultimately
 *   converted to positive integer locations by `vec_as_location()`.
 *
 * - Condition indices are logical vectors the same size as `x`, where `TRUE`
 *   denotes that you should assign to that spot. They are not converted to
 *   integer locations before assignment.
 *
 * The difference between logical location vectors and logical condition vectors
 * is how the associated `value` is treated:
 *
 * - With logical location vectors, `value` must be size 1 or the same size as
 *   `which(index)`, which matches base R `[<-` rules. These are used by
 *   `vec_assign()`.
 *
 * - With logical condition vectors, `value` must be size 1 or the same size as
 *   `index` itself, which matches `dplyr::case_when()` or `dplyr::if_else()`
 *   style usage. These are used by `vec_assign_condition()`.
 */
enum vctrs_assignment_style {
  VCTRS_ASSIGNMENT_STYLE_location,
  VCTRS_ASSIGNMENT_STYLE_condition
};

struct vec_assign_opts {
  bool assign_names;
  bool ignore_outer_names;
  enum vctrs_ownership ownership;
  enum vctrs_assignment_style style;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
  struct r_lazy call;
};

struct vec_proxy_assign_opts {
  bool assign_names;
  bool ignore_outer_names;
  enum vctrs_ownership ownership;
  enum vctrs_assignment_style style;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
  struct r_lazy call;
  // Whether the `proxy` was proxied recursively or not
  bool recursively_proxied;
};

r_obj* vec_assign_opts(r_obj* x,
                       r_obj* index,
                       r_obj* value,
                       const struct vec_assign_opts* p_opts);

r_obj* vec_proxy_assign_opts(r_obj* proxy,
                             r_obj* index,
                             r_obj* value,
                             const struct vec_proxy_assign_opts* p_opts);

r_obj* chr_assign(r_obj* out,
                  r_obj* index,
                  r_obj* value,
                  const enum vctrs_ownership ownership,
                  const enum vctrs_assignment_style style);

r_obj* list_assign(r_obj* out,
                   r_obj* index,
                   r_obj* value,
                   const enum vctrs_ownership ownership,
                   const enum vctrs_assignment_style style);

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
  enum vctrs_assignment_style style
);

void vec_check_condition_index(
  r_obj* index,
  r_ssize size,
  struct vctrs_arg* p_index_arg,
  struct r_lazy call
);

enum vctrs_assignment_style parse_assignment_style(
  r_obj* x,
  const char* x_arg,
  struct r_lazy call
);

#endif
