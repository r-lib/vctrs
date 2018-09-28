#include "vctrs.h"

bool vec_unspecified(SEXP x) {
  if (TYPEOF(x) != LGLSXP)
    return false;

  int n = Rf_length(x);
  if (n == 0)
    return false;

  SEXP dims = Rf_getAttrib(x, R_DimSymbol);
  if (dims != R_NilValue)
    return false;

  int* p_x = LOGICAL(x);
  for (int i = 0; i < n; ++i) {
    if (p_x[i] != NA_LOGICAL)
      return false;
  }

  return true;
}

// R interface -----------------------------------------------------------------

SEXP vctrs_is_unspecified(SEXP x) {
  return Rf_ScalarLogical(vec_unspecified(x));
}
