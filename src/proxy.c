#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;
SEXP syms_vec_proxy_equal_dispatch = NULL;
SEXP fns_vec_proxy_equal_dispatch = NULL;

// Defined below
SEXP vec_proxy_method(SEXP x);
SEXP vec_proxy_invoke(SEXP x, SEXP method);
static SEXP vec_proxy_unwrap(SEXP x);


// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy(SEXP x) {
  int nprot = 0;
  struct vctrs_type_info info = vec_type_info(x);
  PROTECT_TYPE_INFO(&info, &nprot);

  SEXP out;
  if (info.type == vctrs_type_s3) {
    out = vec_proxy_invoke(x, info.proxy_method);
  } else {
    out = x;
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_equal(SEXP x) {
  SEXP proxy = PROTECT(vec_proxy_recursive(x, vctrs_proxy_kind_equal));

  if (is_data_frame(proxy)) {
    // Flatten df-cols so we don't have to recurse to work with data
    // frame proxies
    proxy = PROTECT(df_flatten(proxy));

    // Unwrap data frames of size 1 since the data frame wrapper
    // doesn't impact rowwise equality or identity
    proxy = vec_proxy_unwrap(proxy);

    UNPROTECT(1);
  }

  UNPROTECT(1);
  return proxy;
}
static SEXP vec_proxy_unwrap(SEXP x) {
  if (TYPEOF(x) == VECSXP && XLENGTH(x) == 1 && is_data_frame(x)) {
    x = vec_proxy_unwrap(VECTOR_ELT(x, 0));
  }
  return x;
}

// [[ register() ]]
SEXP vctrs_unset_s4(SEXP x) {
  x = r_clone_referenced(x);
  r_unmark_s4(x);
  return x;
}

SEXP vec_proxy_equal_dispatch(SEXP x) {
  if (vec_typeof(x) == vctrs_type_s3) {
    return vctrs_dispatch1(syms_vec_proxy_equal_dispatch, fns_vec_proxy_equal_dispatch,
                           syms_x, x);
  } else {
    return x;
  }
}

// [[ include("vctrs.h") ]]
SEXP vec_proxy_recursive(SEXP x, enum vctrs_proxy_kind kind) {
  switch (kind) {
  case vctrs_proxy_kind_default: x = vec_proxy(x); break;
  case vctrs_proxy_kind_equal: x = vec_proxy_equal_dispatch(x); break;
  case vctrs_proxy_kind_compare: Rf_error("Internal error: Unimplemented proxy kind");
  }
  PROTECT(x);

  if (is_data_frame(x)) {
    x = PROTECT(r_clone_referenced(x));
    R_len_t n = Rf_length(x);

    for (R_len_t i = 0; i < n; ++i) {
      SEXP col = vec_proxy_recursive(VECTOR_ELT(x, i), kind);
      SET_VECTOR_ELT(x, i, col);
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
  return x;
}

// [[ register() ]]
SEXP vctrs_proxy_recursive(SEXP x, SEXP kind_) {
  enum vctrs_proxy_kind kind;
  if (kind_ == Rf_install("default")) {
    kind = vctrs_proxy_kind_default;
  } else if (kind_ == Rf_install("equal")) {
    kind = vctrs_proxy_kind_equal;
  } else if (kind_ == Rf_install("compare")) {
    kind = vctrs_proxy_kind_compare;
  } else {
    Rf_error("Internal error: Unexpected proxy kind `%s`.", CHAR(PRINTNAME(kind_)));
  }

  return vec_proxy_recursive(x, kind);
}

SEXP vec_proxy_method(SEXP x) {
  return s3_find_method("vec_proxy", x, vctrs_method_table);
}

// This should be faster than normal dispatch but also means that
// proxy methods can't call `NextMethod()`. This could be changed if
// it turns out a problem.
SEXP vec_proxy_invoke(SEXP x, SEXP method) {
  if (method == R_NilValue) {
    return x;
  } else {
    return vctrs_dispatch1(syms_vec_proxy, method, syms_x, x);
  }
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");
  syms_vec_proxy_equal_dispatch = Rf_install("vec_proxy_equal_dispatch");

  fns_vec_proxy_equal_dispatch = r_env_get(ns, syms_vec_proxy_equal_dispatch);
}
