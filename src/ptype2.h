#ifndef VCTRS_PTYPE2_H
#define VCTRS_PTYPE2_H


struct ptype2_opts {
  SEXP x;
  SEXP y;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* y_arg;
  bool df_fallback;
};

SEXP vec_ptype2_dispatch(const struct ptype2_opts* opts,
                         enum vctrs_type x_type,
                         enum vctrs_type y_type,
                         int* left);

SEXP vec_ptype2_opts(const struct ptype2_opts* opts,
                     int* left);

static inline
SEXP vec_ptype2_params(SEXP x,
                       SEXP y,
                       struct vctrs_arg* x_arg,
                       struct vctrs_arg* y_arg,
                       bool df_fallback,
                       int* left) {
  const struct ptype2_opts opts = {
    .x = x,
    .y = y,
    .x_arg = x_arg,
    .y_arg = y_arg,
    .df_fallback = df_fallback
  };
  return vec_ptype2_opts(&opts, left);
}

static inline
SEXP vec_ptype2(SEXP x,
                SEXP y,
                struct vctrs_arg* x_arg,
                struct vctrs_arg* y_arg,
                int* left) {
  const struct ptype2_opts opts = {
    .x = x,
    .y = y,
    .x_arg = x_arg,
    .y_arg = y_arg
  };
  return vec_ptype2_opts(&opts, left);
}

SEXP vec_ptype2_dispatch_s3(const struct ptype2_opts* opts);

bool vec_is_coercible(const struct ptype2_opts* opts, int* dir);


#endif
