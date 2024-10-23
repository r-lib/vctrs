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
  struct fallback_opts fallback;
};

// FIXME: Should we merge these two structs?
static inline
struct ptype2_opts cast_opts_as_ptype2_opts(const struct cast_opts* p_opts) {
  return (struct ptype2_opts) {
    .x = p_opts->x,
    .y = p_opts->to,
    .p_x_arg = p_opts->p_x_arg,
    .p_y_arg = p_opts->p_to_arg,
    .call = p_opts->call,
    .fallback = p_opts->fallback,
  };
}

struct cast_common_opts {
  struct vctrs_arg* p_arg;
  struct r_lazy call;
  struct fallback_opts fallback;
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
    .fallback = {
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
                        const struct fallback_opts* p_opts);


#endif
