#ifndef VCTRS_ASSERT_H
#define VCTRS_ASSERT_H

#include <rlang.h>
#include "vctrs.h"

void vec_assert(r_obj* x, r_ssize size, struct vctrs_arg* arg);
void vec_assert_vector(r_obj* x,
                       struct vctrs_arg* arg,
                       struct r_lazy call);
void vec_assert_size(r_obj* x, r_ssize size, struct vctrs_arg* arg);

#endif
