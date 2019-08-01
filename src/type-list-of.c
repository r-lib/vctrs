#include "vctrs.h"
#include "utils.h"

// [[ include("utils.h") ]]
SEXP new_list_of(SEXP x, SEXP ptype) {
  if (TYPEOF(x) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `x` must be a list.");
  }

  if (vec_size(ptype) != 0) {
    Rf_errorcall(R_NilValue, "Internal error: `ptype` must be a prototype with size 0.");
  }

  x = PROTECT(r_maybe_duplicate(x));

  init_list_of(x, ptype);

  UNPROTECT(1);
  return x;
}

// [[ include("utils.h") ]]
void init_list_of(SEXP x, SEXP ptype) {
  Rf_setAttrib(x, R_ClassSymbol, classes_list_of);
  Rf_setAttrib(x, syms_ptype, ptype);
}
