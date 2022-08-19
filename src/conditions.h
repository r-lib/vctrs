#ifndef VCTRS_CONDITIONS_H
#define VCTRS_CONDITIONS_H

#include "vctrs-core.h"

r_no_return
void stop_incompatible_size(r_obj* x,
                            r_obj* y,
                            r_ssize x_size,
                            r_ssize y_size,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg,
                            struct r_lazy call);

#endif
