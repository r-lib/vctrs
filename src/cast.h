#ifndef VCTRS_CAST_H
#define VCTRS_CAST_H

#include "vctrs-core.h"
#include "ptype2.h"

struct cast_opts {
  r_obj* x;
  r_obj* to;
  struct vctrs_arg* p_x_arg;
  struct vctrs_arg* p_to_arg;
  struct r_lazy call;
  enum s3_fallback s3_fallback;
  // Leave the data frame columns of `to` that `x` lacks as `NULL` instead of
  // filling them with missing values. Only for callers that pass the result
  // straight to `df_assign()`, which skips them, writing each output row once
  // into an output that is already missing.
  bool missing_cols_as_null;
};

struct cast_common_opts {
  struct vctrs_arg* p_arg;
  struct r_lazy call;
  enum s3_fallback s3_fallback;
  bool missing_cols_as_null;
};

r_obj* vec_cast_opts(const struct cast_opts* opts);

static inline
r_obj* vec_cast(r_obj* x,
                r_obj* to,
                struct vctrs_arg* p_x_arg,
                struct vctrs_arg* p_to_arg,
                struct r_lazy call) {
  struct cast_opts opts = {
    .x = x,
    .to = to,
    .p_x_arg = p_x_arg,
    .p_to_arg = p_to_arg,
    .call = call
  };
  return vec_cast_opts(&opts);
}

static inline
r_obj* vec_cast_params(r_obj* x,
                       r_obj* to,
                       struct vctrs_arg* p_x_arg,
                       struct vctrs_arg* p_to_arg,
                       struct r_lazy call,
                       enum s3_fallback s3_fallback) {
  const struct cast_opts opts = {
    .x = x,
    .to = to,
    .p_x_arg = p_x_arg,
    .p_to_arg = p_to_arg,
    .call = call,
    .s3_fallback = s3_fallback
  };
  return vec_cast_opts(&opts);
}

r_obj* vec_cast_common(r_obj* xs,
                       r_obj* to,
                       struct vctrs_arg* p_arg,
                       struct r_lazy call);

r_obj* vec_cast_common_opts(r_obj* xs,
                            r_obj* to,
                            const struct cast_common_opts* opts);

r_obj* vec_cast_common_params(r_obj* xs,
                              r_obj* to,
                              enum s3_fallback s3_fallback,
                              struct vctrs_arg* p_arg,
                              struct r_lazy call);

struct cast_opts new_cast_opts(r_obj* x,
                               r_obj* y,
                               struct vctrs_arg* p_x_arg,
                               struct vctrs_arg* p_y_arg,
                               struct r_lazy call,
                               r_obj* opts);

r_obj* vec_cast_e(const struct cast_opts* opts,
                  ERR* err);

r_obj* vec_cast_default(r_obj* x,
                        r_obj* to,
                        struct vctrs_arg* p_x_arg,
                        struct vctrs_arg* p_to_arg,
                        struct r_lazy call,
                        enum s3_fallback s3_fallback);


#endif
