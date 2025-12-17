#ifndef VCTRS_TYPE_FACTOR_H
#define VCTRS_TYPE_FACTOR_H

#include "vctrs-core.h"
#include "ptype2.h"

SEXP fct_ptype2(
  SEXP x,
  SEXP y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
);

r_obj* ord_ptype2(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback
);

SEXP ord_as_ordered(const struct cast_opts* opts);

#endif
