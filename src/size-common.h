#ifndef VCTRS_SIZE_COMMON_H
#define VCTRS_SIZE_COMMON_H

#include "vctrs-core.h"

r_ssize vec_size_common(r_obj* xs, r_ssize absent);
r_obj* vec_recycle_common(r_obj* xs, r_ssize size);

struct size_common_opts {
  struct vctrs_arg* p_arg;
  struct r_lazy call;
};

r_ssize vec_size_common_opts(r_obj* xs,
                             r_ssize absent,
                             const struct size_common_opts* opts);

r_obj* vec_recycle_common_opts(r_obj* xs,
                               r_ssize size,
                               const struct size_common_opts* opts);

#endif
