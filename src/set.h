#ifndef VCTRS_SET_H
#define VCTRS_SET_H

#include "vctrs-core.h"

r_obj* vec_set_intersect(r_obj* x,
                         r_obj* y,
                         r_obj* ptype,
                         struct vctrs_arg* x_arg,
                         struct vctrs_arg* y_arg,
                         struct r_lazy call);

r_obj* vec_set_difference(r_obj* x,
                          r_obj* y,
                          r_obj* ptype,
                          struct vctrs_arg* x_arg,
                          struct vctrs_arg* y_arg,
                          struct r_lazy call);

r_obj* vec_set_union(r_obj* x,
                     r_obj* y,
                     r_obj* ptype,
                     struct vctrs_arg* x_arg,
                     struct vctrs_arg* y_arg,
                     struct r_lazy call);

#endif
