#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;
SEXP fns_vec_proxy = NULL;


SEXP vec_proxy(SEXP x) {
  return vctrs_dispatch1(syms_vec_proxy, fns_vec_proxy,
                         syms_x, x);
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");
  fns_vec_proxy = Rf_findVar(syms_vec_proxy, ns);
}
