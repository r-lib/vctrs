#ifndef VCTRS_PTYPE2_H
#define VCTRS_PTYPE2_H


// Sync with R constants in ptype2.R
enum df_fallback {
  DF_FALLBACK_DEFAULT = 0,
  DF_FALLBACK_NONE = 1,
  DF_FALLBACK_WARN,
  DF_FALLBACK_WARN_MAYBE,
  DF_FALLBACK_QUIET = 255
};

enum s3_fallback {
  S3_FALLBACK_false = 0,
  S3_FALLBACK_true
};

struct ptype2_opts {
  SEXP x;
  SEXP y;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* y_arg;
  enum df_fallback df_fallback;
  enum s3_fallback s3_fallback;
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
                       enum df_fallback df_fallback,
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

struct ptype2_opts new_ptype2_opts(SEXP x,
                                   SEXP y,
                                   struct vctrs_arg* x_arg,
                                   struct vctrs_arg* y_arg,
                                   SEXP opts);


#endif
