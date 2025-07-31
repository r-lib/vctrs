#ifndef VCTRS_ASSERT_H
#define VCTRS_ASSERT_H

#include "vctrs-core.h"

void obj_check_vector(r_obj* x,
                      struct vctrs_arg* arg,
                      struct r_lazy call);

void vec_check_size(r_obj* x,
                    r_ssize size,
                    struct vctrs_arg* arg,
                    struct r_lazy call);

void obj_check_list(r_obj* x,
                    struct vctrs_arg* arg,
                    struct r_lazy call);

void list_check_all_vectors(r_obj* xs,
                            struct vctrs_arg* p_arg,
                            struct r_lazy call);

void list_check_all_size(r_obj* xs,
                         r_ssize size,
                         struct vctrs_arg* p_arg,
                         struct r_lazy call);

void list_check_all_recyclable(r_obj* xs,
                               r_ssize size,
                               struct vctrs_arg* p_arg,
                               struct r_lazy call);

#endif
