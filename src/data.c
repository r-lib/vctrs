#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy_dispatch = NULL;
SEXP fns_vec_proxy_dispatch = NULL;


static SEXP vec_proxy_dispatch(SEXP x) {
  return vctrs_dispatch1(syms_vec_proxy_dispatch, fns_vec_proxy_dispatch,
                         syms_x, x);
}

// [[register]]
SEXP vec_proxy(SEXP x) {
  if (vec_typeof(x) == vctrs_type_s3) {
    return vec_proxy_dispatch(x);
  } else {
    return x;
  }
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy_dispatch = Rf_install("vec_proxy_dispatch");
  fns_vec_proxy_dispatch = Rf_findVar(syms_vec_proxy_dispatch, ns);
}
