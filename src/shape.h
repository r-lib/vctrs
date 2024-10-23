#ifndef VCTRS_SHAPE_H
#define VCTRS_SHAPE_H

#include "vctrs-core.h"
#include "cast.h"


SEXP vec_shaped_ptype(SEXP ptype,
                      SEXP x, SEXP y,
                      struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);

r_obj* vec_shape_broadcast(r_obj* out, const struct cast_opts* p_opts);


#endif
