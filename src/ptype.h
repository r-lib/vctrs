#ifndef VCTRS_PTYPE_H
#define VCTRS_PTYPE_H

#include "vctrs-core.h"

r_obj* vec_ptype(r_obj* x, struct vctrs_arg* x_arg, struct r_lazy call);
r_obj* vec_ptype_final(r_obj* x, struct vctrs_arg* x_arg, struct r_lazy call);

#endif
