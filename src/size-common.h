#ifndef VCTRS_SIZE_COMMON_H
#define VCTRS_SIZE_COMMON_H

#include "vctrs-core.h"

r_ssize vec_size_common(r_obj* xs, r_ssize absent);
r_obj* vec_recycle_common(r_obj* xs, r_ssize size);

#endif
