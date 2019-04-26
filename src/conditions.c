#include "vctrs.h"
#include "utils.h"


void stop_scalar_type(SEXP x, struct vctrs_arg* arg) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_scalar_type"),
                               PROTECT(r_protect(x)),
                               PROTECT(vctrs_arg(arg))));
  Rf_eval(call, vctrs_ns_env);
  Rf_error("Internal error: `stop_scalar_type()` should have jumped");
}

void vec_assert(SEXP x, struct vctrs_arg* arg) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg);
  }
}
