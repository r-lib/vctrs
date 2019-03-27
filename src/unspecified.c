#include "vctrs.h"

bool vec_is_unspecified(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    return false;
  }

  R_len_t n = Rf_length(x);
  if (n == 0) {
    return false;
  }

  if (has_dim(x)) {
    return false;
  }

  R_len_t* p_x = LOGICAL(x);
  for (R_len_t i = 0; i < n; ++i) {
    if (p_x[i] != NA_LOGICAL) {
      return false;
    }
  }

  return true;
}

// R interface -----------------------------------------------------------------

SEXP vctrs_is_unspecified(SEXP x) {
  return Rf_ScalarLogical(vec_is_unspecified(x));
}
