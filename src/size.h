#ifndef VCTRS_SIZE_H
#define VCTRS_SIZE_H

#include "vctrs-core.h"

r_ssize vec_size(r_obj* x);

r_ssize vec_as_short_length(r_obj* size,
                            struct vctrs_arg* p_arg,
                            struct r_lazy call);

r_ssize vec_as_ssize(r_obj* n,
                     struct vctrs_arg* arg,
                     struct r_lazy call);

r_obj* vec_recycle2(r_obj* x,
                    r_ssize size,
                    struct vctrs_arg* x_arg,
                    struct r_lazy call);

// FIXME: Pass error call everywhere
static inline
r_obj* vec_recycle(r_obj* x,
                   r_ssize size,
                   struct vctrs_arg* x_arg) {
  return vec_recycle2(x, size, x_arg, r_lazy_null);
}

r_obj* vec_recycle_fallback(r_obj* x,
                            r_ssize size,
                            struct vctrs_arg* x_arg);

r_ssize df_size(r_obj* x);
r_ssize df_raw_size(r_obj* x);
r_ssize df_rownames_size(r_obj* x);
r_ssize df_raw_size_from_list(r_obj* x);

#endif
