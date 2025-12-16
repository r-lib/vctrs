#ifndef VCTRS_TYPE_TIBBLE_H
#define VCTRS_TYPE_TIBBLE_H

#include "vctrs-core.h"
#include "ptype2.h"


r_obj* tib_ptype2(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback
);

SEXP tib_cast(const struct cast_opts* opts);


#endif
