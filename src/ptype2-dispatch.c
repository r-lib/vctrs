#include "vctrs.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_dispatch(SEXP x, SEXP y,
                         enum vctrs_type x_type,
                         enum vctrs_type y_type,
                         struct vctrs_arg* x_arg,
                         struct vctrs_arg* y_arg,
                         int* left) {
  enum vctrs_type2_s3 type2_s3 = vec_typeof2_s3_impl(x, y, x_type, y_type, left);

  switch (type2_s3) {
  case vctrs_type2_s3_character_bare_factor:
  case vctrs_type2_s3_character_bare_ordered:
    return vctrs_shared_empty_chr;

  case vctrs_type2_s3_bare_factor_bare_factor:
    return fct_ptype2(x, y, x_arg, y_arg);

  case vctrs_type2_s3_bare_ordered_bare_ordered:
    return ord_ptype2(x, y, x_arg, y_arg);

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
    return tib_ptype2(x, y, x_arg, y_arg);

  default:
    return vec_ptype2_dispatch_s3(x, y, x_arg, y_arg);
  }
}

// Initialised at load time
static SEXP fns_vec_ptype2_dispatch_s3 = NULL;
static SEXP syms_vec_ptype2_dispatch_s3 = NULL;

static inline SEXP vec_ptype2_default(SEXP x,
                                      SEXP y,
                                      SEXP x_arg,
                                      SEXP y_arg) {
  return vctrs_eval_mask4(Rf_install("vec_default_ptype2"),
                          syms_x, x,
                          syms_y, y,
                          syms_x_arg, x_arg,
                          syms_y_arg, y_arg,
                          vctrs_ns_env);
}

static SEXP get_ptype2_method(SEXP x,
                              const char* generic,
                              SEXP table,
                              SEXP* method_sym_out) {
  SEXP class = R_NilValue;
  if (OBJECT(x)) {
    class = Rf_getAttrib(x, R_ClassSymbol);
  }

  // This also handles gremlins objects where `x` is an OBJECT(), but
  // the class is NULL
  if (class == R_NilValue) {
    class = s3_dispatch_class(x);
  }
  PROTECT(class);

  SEXP* class_ptr = STRING_PTR(class);
  int n_class = Rf_length(class);

  // FIXME: Disable inheritance
  for (int i = 0; i < n_class; ++i, ++class_ptr) {
    SEXP method_sym = s3_paste_method_sym(generic, CHAR(*class_ptr));
    SEXP method = s3_sym_get_method(method_sym, table);
    if (method != R_NilValue) {
      UNPROTECT(1);
      *method_sym_out = method_sym;
      return method;
    }
  }

  UNPROTECT(1);
  *method_sym_out = R_NilValue;
  return R_NilValue;
}

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_dispatch_s3(SEXP x,
                            SEXP y,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg) {
  SEXP x_arg_obj = PROTECT(vctrs_arg(x_arg));
  SEXP y_arg_obj = PROTECT(vctrs_arg(y_arg));

  SEXP x_method_sym = R_NilValue;
  SEXP x_method = PROTECT(get_ptype2_method(x,
                                            "vec_ptype2",
                                            vctrs_method_table,
                                            &x_method_sym));

  if (x_method == R_NilValue) {
    SEXP out = vec_ptype2_default(x, y, x_arg_obj, y_arg_obj);
    UNPROTECT(3);
    return out;
  }

  const char* x_method_str = CHAR(PRINTNAME(x_method_sym));
  SEXP x_table = s3_get_table(CLOENV(x_method));

  SEXP y_method_sym = R_NilValue;
  SEXP y_method = get_ptype2_method(y,
                                    x_method_str,
                                    x_table,
                                    &y_method_sym);

  // FIXME: The `AsIs` class relies on a default ptype2 method
  if (y_method == R_NilValue) {
    y_method_sym = s3_paste_method_sym(x_method_str, "default");
    y_method = s3_sym_get_method(y_method_sym, x_table);
  }

  if (y_method == R_NilValue) {
    SEXP out = vec_ptype2_default(x, y, x_arg_obj, y_arg_obj);
    UNPROTECT(3);
    return out;
  }

  PROTECT(y_method);

  SEXP out = vctrs_dispatch4(y_method_sym, y_method,
                             syms_x, x,
                             syms_y, y,
                             syms_x_arg, x_arg_obj,
                             syms_y_arg, y_arg_obj);

  UNPROTECT(4);
  return out;
}


void vctrs_init_ptype2_dispatch(SEXP ns) {
  syms_vec_ptype2_dispatch_s3 = Rf_install("vec_ptype2_dispatch_s3");
  fns_vec_ptype2_dispatch_s3 = Rf_findVar(syms_vec_ptype2_dispatch_s3, ns);
}
