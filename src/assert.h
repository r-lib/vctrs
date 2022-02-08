#ifndef VCTRS_ASSERT_H
#define VCTRS_ASSERT_H

void vec_assert(r_obj* x, r_ssize size, struct vctrs_arg* arg);

void vec_check_vector(r_obj* x,
                      struct vctrs_arg* arg,
                      struct r_lazy call);

void vec_check_size(r_obj* x, r_ssize size, struct vctrs_arg* arg);

#endif
