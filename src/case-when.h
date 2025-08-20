#ifndef VCTRS_CASE_WHEN_H
#define VCTRS_CASE_WHEN_H

#include "vctrs-core.h"
#include "optional.h"
#include "c-unchop.h"

r_obj* vec_case_when(
  r_obj* cases,
  r_obj* values,
  r_obj* default_,
  enum list_unchop_unmatched unmatched,
  r_obj* ptype,
  struct optional_r_ssize size,
  struct vctrs_arg* p_cases_arg,
  struct vctrs_arg* p_values_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy call
);

#endif
