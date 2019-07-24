#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;
SEXP syms_vec_proxy_equal = NULL;
SEXP fns_vec_proxy_equal = NULL;

// Defined below
SEXP vec_proxy_method(SEXP x);
SEXP vec_proxy_invoke(SEXP x, SEXP method);


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

// [[ include("vctrs.h") ]]
SEXP vec_proxy_recursive(SEXP x) {
  x = PROTECT(vec_proxy(x));

  if (is_data_frame(x)) {
    x = PROTECT(r_maybe_duplicate(x));
    R_len_t n = Rf_length(x);

    for (R_len_t i = 0; i < n; ++i) {
      SEXP col = vec_proxy_recursive(VECTOR_ELT(x, i));
      SET_VECTOR_ELT(x, i, col);
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
  return x;
}

SEXP vec_proxy_method(SEXP x) {
  return s3_find_method("vec_proxy", x);
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


// [[ include("vctrs.h") ]]
SEXP vec_proxy_equal(SEXP x) {
  if (vec_typeof(x) == vctrs_type_s3) {
    return vctrs_dispatch1(syms_vec_proxy_equal, fns_vec_proxy_equal,
                           syms_x, x);
  } else {
    return x;
  }
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");
  syms_vec_proxy_equal = Rf_install("vec_proxy_equal");

  fns_vec_proxy_equal = r_env_get(ns, syms_vec_proxy_equal);
}
