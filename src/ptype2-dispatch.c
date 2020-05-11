#include "vctrs.h"
#include "ptype2.h"
#include "type-data-frame.h"
#include "type-factor.h"
#include "type-tibble.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_dispatch(const struct ptype2_opts* opts,
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
    return vec_ptype2_dispatch_s3(opts);
  }
}

// Initialised at load time
static SEXP syms_vec_ptype2_default = NULL;

static inline
SEXP vec_ptype2_default(SEXP x,
                        SEXP y,
                        SEXP x_arg,
                        SEXP y_arg,
                        const struct ptype2_opts* opts) {
  SEXP df_fallback_obj = PROTECT(r_int(opts->df_fallback));
  SEXP s3_fallback_obj = PROTECT(r_int(opts->s3_fallback));
  SEXP out = vctrs_eval_mask7(syms_vec_ptype2_default,
                              syms_x, x,
                              syms_y, y,
                              syms_x_arg, x_arg,
                              syms_y_arg, y_arg,
                              syms_from_dispatch, vctrs_shared_true,
                              syms_df_fallback, df_fallback_obj,
                              Rf_install("vctrs:::s3_fallback"), s3_fallback_obj,
                              vctrs_ns_env);
  UNPROTECT(2);
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
  SEXP x = PROTECT(vec_ptype(opts->x, opts->x_arg));
  SEXP y = PROTECT(vec_ptype(opts->y, opts->y_arg));

  SEXP x_arg_obj = PROTECT(vctrs_arg(opts->x_arg));
  SEXP y_arg_obj = PROTECT(vctrs_arg(opts->y_arg));

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
    SEXP out = vec_ptype2_default(x, y, x_arg_obj, y_arg_obj, opts);
    UNPROTECT(5);
    return out;
  }

  SEXP out = vctrs_dispatch4(method_sym, method,
                             syms_x, x,
                             syms_y, y,
                             syms_x_arg, x_arg_obj,
                             syms_y_arg, y_arg_obj);

  UNPROTECT(5);
  return out;
}


void vctrs_init_ptype2_dispatch(SEXP ns) {
  syms_vec_ptype2_default = Rf_install("vec_default_ptype2");
}
