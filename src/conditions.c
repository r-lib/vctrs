#include "vctrs.h"
#include "utils.h"


void stop_scalar_type(SEXP x, const char* arg) {
  SEXP arg_chr;
  if (strlen(arg)) {
    arg_chr = PROTECT(Rf_mkString(arg));
  } else {
    arg_chr = PROTECT(R_NilValue);
  }

  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_scalar_type"),
                               r_protect(x),
                               arg_chr));
  Rf_eval(call, vctrs_ns_env);
  Rf_error("Internal error: `stop_scalar_type()` should have jumped");
}

void vec_assert(SEXP x, const char* arg) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg);
  }
}
