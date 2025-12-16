#ifndef VCTRS_TYPE_FACTOR_H
#define VCTRS_TYPE_FACTOR_H

#include "vctrs-core.h"

SEXP fct_ptype2(
  SEXP x,
  SEXP y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
);

SEXP ord_ptype2(const struct ptype2_opts* opts);
SEXP ord_as_ordered(const struct cast_opts* opts);

#endif
