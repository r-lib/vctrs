#ifndef VCTRS_CAST_H
#define VCTRS_CAST_H

#include "vctrs-core.h"
#include "ptype2.h"

struct cast_opts {
  r_obj* x;
  r_obj* to;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* to_arg;
  struct r_lazy call;
  struct fallback_opts fallback;
};

struct cast_common_opts {
  struct vctrs_arg* p_arg;
  struct r_lazy call;
  struct fallback_opts fallback;
};

r_obj* vec_cast_opts(const struct cast_opts* opts);

static inline
r_obj* vec_cast(r_obj* x,
                r_obj* to,
                struct vctrs_arg* x_arg,
                struct vctrs_arg* to_arg,
                struct r_lazy call) {
  struct cast_opts opts = {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg,
    .call = call
  };
  return vec_cast_opts(&opts);
}

static inline
r_obj* vec_cast_params(r_obj* x,
                       r_obj* to,
                       struct vctrs_arg* x_arg,
                       struct vctrs_arg* to_arg,
                       enum df_fallback df_fallback,
                       enum s3_fallback s3_fallback) {
  const struct cast_opts opts = {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg,
    .fallback = {
      .df = df_fallback,
      .s3 = s3_fallback
    }
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
                              enum df_fallback df_fallback,
                              enum s3_fallback s3_fallback,
                              struct vctrs_arg* p_arg,
                              struct r_lazy call);

struct cast_opts new_cast_opts(r_obj* x,
                               r_obj* y,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* y_arg,
                               struct r_lazy call,
                               r_obj* opts);

r_obj* vec_cast_e(const struct cast_opts* opts,
                  ERR* err);

r_obj* vec_cast_default(r_obj* x,
                        r_obj* y,
                        r_obj* x_arg,
                        r_obj* to_arg,
                        struct r_lazy call,
                        const struct fallback_opts* opts);


#endif
