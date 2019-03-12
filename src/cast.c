#include "vctrs.h"

// Initialised at load time
static SEXP vec_cast_dispatch_fn = NULL;


SEXP vec_cast(SEXP x, SEXP to) {
  if (x == R_NilValue || to == R_NilValue) {
    return x;
  }

 dispatch: {
    SEXP dispatch_call = PROTECT(Rf_lang3(vec_cast_dispatch_fn, x, to));
    SEXP out = Rf_eval(dispatch_call, R_GlobalEnv);

    UNPROTECT(1);
    return out;
  }
}


void vctrs_init_cast(SEXP ns) {
  vec_cast_dispatch_fn = Rf_findVar(Rf_install("vec_cast_dispatch"), ns);
}
