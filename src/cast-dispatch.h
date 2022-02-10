#ifndef VCTRS_CAST_DISPATCH_H
#define VCTRS_CAST_DISPATCH_H

#include "vctrs-core.h"

r_obj* vec_cast_dispatch_native(const struct cast_opts* opts,
                                enum vctrs_type x_type,
                                enum vctrs_type to_type,
                                bool* lossy);

#endif
