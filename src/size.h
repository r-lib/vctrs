#ifndef VCTRS_SIZE_H
#define VCTRS_SIZE_H

#include "vctrs-core.h"
#include "globals.h"

r_ssize vec_size(r_obj* x);
r_ssize vec_size_3(r_obj* x, struct vctrs_arg* p_arg, struct r_lazy call);

r_obj* vec_recycle(
  r_obj* x,
  r_ssize size,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
);

r_obj* vec_recycle_fallback(r_obj* x,
                            r_ssize size,
                            struct vctrs_arg* x_arg,
                            struct r_lazy call);

r_obj* list_sizes(r_obj* x, const struct vec_error_opts* opts);

r_ssize df_size(r_obj* x);
r_ssize df_raw_size(r_obj* x);
r_ssize df_rownames_size(r_obj* x);
r_ssize df_raw_size_from_list(r_obj* x);

r_ssize vec_as_short_length(r_obj* size,
                            struct vctrs_arg* p_arg,
                            struct r_lazy call);

r_ssize vec_as_ssize(r_obj* n,
                     struct vctrs_arg* arg,
                     struct r_lazy call);

#endif
