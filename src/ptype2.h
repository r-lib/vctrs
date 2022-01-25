#ifndef VCTRS_PTYPE2_H
#define VCTRS_PTYPE2_H


// Sync with R constants in ptype2.R

#define DF_FALLBACK_DEFAULT 0

enum df_fallback {
  DF_FALLBACK_warn_maybe = 0,
  DF_FALLBACK_warn,
  DF_FALLBACK_none,
  DF_FALLBACK_quiet
};


#define S3_FALLBACK_DEFAULT 0

enum s3_fallback {
  S3_FALLBACK_false = 0,
  S3_FALLBACK_true
};

struct fallback_opts {
  enum df_fallback df;
  enum s3_fallback s3;
};

struct ptype2_opts {
  r_obj* x;
  r_obj* y;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* y_arg;
  struct r_lazy call;
  struct fallback_opts fallback;
};

SEXP vec_ptype2_dispatch_native(const struct ptype2_opts* opts,
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
    .fallback = {
      .df = df_fallback
    }
  };
  return vec_ptype2_opts(&opts, left);
}

static inline
r_obj* vec_ptype2(r_obj* x,
                  r_obj* y,
                  struct vctrs_arg* x_arg,
                  struct vctrs_arg* y_arg,
                  int* left,
                  struct r_lazy call) {
  const struct ptype2_opts opts = {
    .x = x,
    .y = y,
    .x_arg = x_arg,
    .y_arg = y_arg,
    .call = call
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

SEXP new_fallback_r_opts(const struct ptype2_opts* opts);
struct fallback_opts new_fallback_opts(SEXP opts);

r_obj* vec_invoke_coerce_method(r_obj* method_sym, r_obj* method,
                                r_obj* x_sym, r_obj* x,
                                r_obj* y_sym, r_obj* y,
                                r_obj* x_arg_sym, r_obj* x_arg,
                                r_obj* y_arg_sym, r_obj* y_arg,
                                struct r_lazy call,
                                const struct fallback_opts* opts);

SEXP vec_ptype2_from_unspecified(const struct ptype2_opts* opts,
                                 enum vctrs_type other_type,
                                 SEXP other,
                                 struct vctrs_arg* other_arg);


#endif
