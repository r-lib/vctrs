#include "vctrs.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP tibble_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP out = PROTECT(df_ptype2(x, y, x_arg, y_arg));

  Rf_setAttrib(out, R_ClassSymbol, classes_tibble);

  UNPROTECT(1);
  return out;
}
