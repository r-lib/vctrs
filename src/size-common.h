#ifndef VCTRS_SIZE_COMMON_H
#define VCTRS_SIZE_COMMON_H

#include "vctrs-core.h"

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

static inline
r_ssize vec_size_common(r_obj* xs, r_ssize absent) {
  struct size_common_opts args = {
    .p_arg = vec_args.empty,
    .call = lazy_calls.vec_size_common
  };
  return vec_size_common_opts(xs, absent, &args);
}

static inline
r_obj* vec_recycle_common(r_obj* xs, r_ssize size) {
  struct size_common_opts args = {
    .p_arg = vec_args.empty,
    .call = lazy_calls.vec_recycle_common
  };
  return vec_recycle_common_opts(xs, size, &args);
}



static inline
r_ssize vec_check_size_common(r_obj* xs,
                              r_ssize absent,
                              struct vctrs_arg* p_arg,
                              struct r_lazy call) {
  struct size_common_opts args = {
    .p_arg = p_arg,
    .call = call
  };
  return vec_size_common_opts(xs, absent, &args);
}

static inline
r_obj* vec_check_recycle_common(r_obj* xs,
                                r_ssize size,
                                struct vctrs_arg* p_arg,
                                struct r_lazy call) {
  struct size_common_opts args = {
    .p_arg = p_arg,
    .call = call
  };
  return vec_recycle_common_opts(xs, size, &args);
}

#endif
