#ifndef VCTRS_CASE_WHEN_H
#define VCTRS_CASE_WHEN_H

#include "vctrs-core.h"
#include "list-combine.h"

r_obj* vec_case_when(
  r_obj* cases,
  r_obj* values,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  r_obj* ptype,
  r_ssize size,
  struct vctrs_arg* p_cases_arg,
  struct vctrs_arg* p_values_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
);

r_obj* vec_replace_when(
  r_obj* x,
  r_obj* cases,
  r_obj* values,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_cases_arg,
  struct vctrs_arg* p_values_arg,
  struct r_lazy error_call
);

#endif
