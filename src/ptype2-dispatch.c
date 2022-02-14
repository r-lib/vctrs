#include "vctrs.h"
#include "type-data-frame.h"
#include "type-factor.h"
#include "type-tibble.h"
#include "utils.h"

// [[ include("ptype2.h") ]]
SEXP vec_ptype2_dispatch_native(const struct ptype2_opts* opts,
                                enum vctrs_type x_type,
                                enum vctrs_type y_type,
                                int* left) {
  SEXP x = opts->x;
  SEXP y = opts->y;
  enum vctrs_type2_s3 type2_s3 = vec_typeof2_s3_impl(x, y, x_type, y_type, left);

  switch (type2_s3) {
  case vctrs_type2_s3_character_bare_factor:
  case vctrs_type2_s3_character_bare_ordered:
    return vctrs_shared_empty_chr;

  case vctrs_type2_s3_bare_factor_bare_factor:
    return fct_ptype2(opts);

  case vctrs_type2_s3_bare_ordered_bare_ordered:
    return ord_ptype2(opts);

  case vctrs_type2_s3_bare_date_bare_date:
    return vctrs_shared_empty_date;

  case vctrs_type2_s3_bare_date_bare_posixct:
  case vctrs_type2_s3_bare_date_bare_posixlt:
    return date_datetime_ptype2(x, y);

  case vctrs_type2_s3_bare_posixct_bare_posixct:
  case vctrs_type2_s3_bare_posixct_bare_posixlt:
  case vctrs_type2_s3_bare_posixlt_bare_posixlt:
    return datetime_datetime_ptype2(x, y);

  case vctrs_type2_s3_dataframe_bare_tibble:
  case vctrs_type2_s3_bare_tibble_bare_tibble:
    return tib_ptype2(opts);

  default:
    return R_NilValue;
  }
}

// Initialised at load time
static SEXP syms_vec_ptype2_default = NULL;

static inline
r_obj* vec_ptype2_default(r_obj* x,
                          r_obj* y,
                          r_obj* x_arg,
                          r_obj* y_arg,
                          struct r_lazy call,
                          const struct fallback_opts* opts) {
  r_obj* df_fallback_obj = KEEP(r_int(opts->df));
  r_obj* s3_fallback_obj = KEEP(r_int(opts->s3));
  r_obj* ffi_call = KEEP(r_lazy_eval(call));
  r_obj* out = vctrs_eval_mask8(syms_vec_ptype2_default,
                                syms_x, x,
                                syms_y, y,
                                syms_x_arg, x_arg,
                                syms_y_arg, y_arg,
                                syms_call, ffi_call,
                                syms_from_dispatch, vctrs_shared_true,
                                syms_df_fallback, df_fallback_obj,
                                syms_s3_fallback, s3_fallback_obj);
  FREE(3);
  return out;
}

SEXP find_common_class(SEXP x, SEXP y) {
  SEXP x_class = PROTECT(r_class(x));
  SEXP y_class = PROTECT(r_class(y));

  R_len_t x_n = Rf_length(x_class);
  R_len_t y_n = Rf_length(x_class);

  SEXP const * p_x_classes = STRING_PTR_RO(x_class);
  SEXP const * p_y_classes = STRING_PTR_RO(y_class);

  for (R_len_t i = 0; i < x_n; ++i) {
    for (R_len_t j = 0; j < y_n; ++j) {
      if (p_x_classes[i] == p_y_classes[j]) {
        UNPROTECT(2);
        return p_x_classes[i];
      }
    }
  }

  UNPROTECT(2);
  return R_NilValue;
}

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_dispatch_s3(const struct ptype2_opts* opts) {
  SEXP x = PROTECT(vec_ptype(opts->x, opts->x_arg, opts->call));
  SEXP y = PROTECT(vec_ptype(opts->y, opts->y_arg, opts->call));

  SEXP r_x_arg = PROTECT(vctrs_arg(opts->x_arg));
  SEXP r_y_arg = PROTECT(vctrs_arg(opts->y_arg));

  SEXP method_sym = R_NilValue;
  SEXP method = s3_find_method_xy("vec_ptype2", x, y, vctrs_method_table, &method_sym);

  // Compatibility with legacy double dispatch mechanism
  if (method == R_NilValue) {
    SEXP x_method_sym = R_NilValue;
    SEXP x_method = PROTECT(s3_find_method2("vec_ptype2",
                                             x,
                                             vctrs_method_table,
                                             &x_method_sym));

    if (x_method != R_NilValue) {
      const char* x_method_str = CHAR(PRINTNAME(x_method_sym));
      SEXP x_table = s3_get_table(CLOENV(x_method));

      method = s3_find_method2(x_method_str,
                               y,
                               x_table,
                               &method_sym);
    }

    UNPROTECT(1);
  }

  PROTECT(method);

  if (method == R_NilValue) {
    SEXP out = vec_ptype2_default(x,
                                  y,
                                  r_x_arg,
                                  r_y_arg,
                                  opts->call,
                                  &(opts->fallback));
    UNPROTECT(5);
    return out;
  }

  SEXP out = vec_invoke_coerce_method(method_sym, method,
                                      syms_x, x,
                                      syms_y, y,
                                      syms_x_arg, r_x_arg,
                                      syms_y_arg, r_y_arg,
                                      opts->call,
                                      &(opts->fallback));

  UNPROTECT(5);
  return out;
}

r_obj* vec_invoke_coerce_method(r_obj* method_sym, r_obj* method,
                                r_obj* x_sym, r_obj* x,
                                r_obj* y_sym, r_obj* y,
                                r_obj* x_arg_sym, r_obj* x_arg,
                                r_obj* y_arg_sym, r_obj* y_arg,
                                struct r_lazy lazy_call,
                                const struct fallback_opts* opts) {
  r_obj* call = KEEP(r_lazy_eval(lazy_call));

  if (opts->df != DF_FALLBACK_DEFAULT ||
      opts->s3 != S3_FALLBACK_DEFAULT) {
    r_obj* df_fallback_obj = KEEP(r_int(opts->df));
    r_obj* s3_fallback_obj = KEEP(r_int(opts->s3));

    r_obj* out = vctrs_dispatch7(method_sym, method,
                                 x_sym, x,
                                 y_sym, y,
                                 x_arg_sym, x_arg,
                                 y_arg_sym, y_arg,
                                 syms_call, call,
                                 syms_df_fallback, df_fallback_obj,
                                 syms_s3_fallback, s3_fallback_obj);
    FREE(3);
    return out;
  } else {
    r_obj* out = vctrs_dispatch5(method_sym, method,
                                 x_sym, x,
                                 y_sym, y,
                                 x_arg_sym, x_arg,
                                 y_arg_sym, y_arg,
                                 syms_call, call);
    FREE(1);
    return out;
  }
}

// [[ register() ]]
SEXP vctrs_ptype2_dispatch_native(SEXP x,
                                  SEXP y,
                                  SEXP fallback_opts,
                                  SEXP x_arg,
                                  SEXP y_arg) {
  struct vctrs_arg c_x_arg = vec_as_arg(x_arg);
  struct vctrs_arg c_y_arg = vec_as_arg(y_arg);

  const struct ptype2_opts c_opts = new_ptype2_opts(x, y, &c_x_arg, &c_y_arg, fallback_opts);

  int _left;

  SEXP out = vec_ptype2_dispatch_native(&c_opts, vec_typeof(x), vec_typeof(y), &_left);

  if (out == R_NilValue) {
    return vec_ptype2_default(x,
                              y,
                              x_arg,
                              y_arg,
                              c_opts.call,
                              &c_opts.fallback);
  } else {
    return out;
  }
}


void vctrs_init_ptype2_dispatch(SEXP ns) {
  syms_vec_ptype2_default = Rf_install("vec_default_ptype2");
}
