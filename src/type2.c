#include "vctrs.h"

// Initialised at load time
static SEXP vec_type2_dispatch_fn = NULL;


SEXP vctrs_type2(SEXP x, SEXP y) {
  SEXP dispatch_call = PROTECT(Rf_lang3(vec_type2_dispatch_fn, x, y));
  SEXP out = Rf_eval(dispatch_call, R_GlobalEnv);

  UNPROTECT(1);
  return out;
}


void vctrs_init_type2(SEXP ns) {
  vec_type2_dispatch_fn = Rf_findVar(Rf_install("vec_type2_dispatch"), ns);
}
