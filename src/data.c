#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;
SEXP syms_vec_proxy_dispatch = NULL;
SEXP fns_vec_proxy_dispatch = NULL;


static SEXP vec_proxy_dispatch(SEXP x) {
  return vctrs_dispatch1(syms_vec_proxy_dispatch, fns_vec_proxy_dispatch,
                         syms_x, x);
}

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy(SEXP x) {
  if (vec_typeof(x) == vctrs_type_s3) {
    return vec_proxy_dispatch(x);
  } else {
    return x;
  }
}
// [[ include("vctrs.h") ]]
SEXP vec_proxy_method(SEXP x) {
  return s3_find_method(x, "vec_proxy");
}
// [[ include("vctrs.h") ]]
SEXP vec_proxy_invoke(SEXP x, SEXP method) {
  return vctrs_dispatch1(syms_vec_proxy, method,
                         syms_x, x);
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");

  syms_vec_proxy_dispatch = Rf_install("vec_proxy_dispatch");
  fns_vec_proxy_dispatch = Rf_findVar(syms_vec_proxy_dispatch, ns);
}
