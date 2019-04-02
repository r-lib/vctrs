#include "vctrs.h"
#include "utils.h"


void stop_scalar_type(SEXP x, const char* arg_str) {
  SEXP arg;
  if (!strlen(arg_str)) {
    arg = PROTECT(Rf_mkString(arg_str));
  } else {
    arg = PROTECT(R_NilValue);
  }

  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_scalar_type"),
                               r_protect(x),
                               arg));
  Rf_eval(call, vctrs_ns_env);
  Rf_error("Internal error: `stop_scalar_type()` should have jumped");
}
