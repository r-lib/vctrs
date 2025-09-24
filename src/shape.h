#ifndef VCTRS_SHAPE_H
#define VCTRS_SHAPE_H

#include "vctrs-core.h"
#include "cast.h"

// Computes the common shape of `x` and `y` and attaches it as the
// dimensions of `ptype`. If `x` and `y` are both atomic with `NULL` dimensions,
// then no dimensions are attached and `ptype` is returned unmodified.
r_obj* vec_shaped_ptype(
  r_obj* ptype,
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
);

r_obj* vec_shape_broadcast(r_obj* out, const struct cast_opts* p_opts);


#endif
