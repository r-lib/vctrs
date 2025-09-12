#ifndef VCTRS_IF_ELSE_H
#define VCTRS_IF_ELSE_H

#include "vctrs-core.h"

r_obj* vec_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* ptype,
  struct vctrs_arg* p_condition_arg,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call
);

#endif
