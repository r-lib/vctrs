#include "vctrs.h"

bool is_bool(SEXP x) {
  return
    TYPEOF(x) == LGLSXP &&
    Rf_length(x) == 1 &&
    *LOGICAL(x) != NA_LOGICAL;
}

// From rlang. These ignore missing values.
R_len_t r_lgl_sum(SEXP lgl) {
  if (TYPEOF(lgl) != LGLSXP) {
    Rf_errorcall(R_NilValue, "Internal error: Excepted logical vector in `r_lgl_sum()`");
  }

  R_len_t n = Rf_length(lgl);

  R_len_t sum = 0;
  int* ptr = LOGICAL(lgl);

  for (R_len_t i = 0; i < n; ++i, ++ptr) {
    // This can't overflow since `sum` is necessarily smaller or equal
    // to the vector length expressed in `R_len_t`.
    int elt = *ptr;
    if (elt != NA_LOGICAL) {
      sum += elt;
    }
  }

  return sum;
}

SEXP r_lgl_which(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    Rf_errorcall(R_NilValue, "Internal error: Expected logical vector in `r_lgl_which()`");
  }

  R_len_t n = Rf_length(x);
  int* data = LOGICAL(x);

  R_len_t which_n = r_lgl_sum(x);
  SEXP which = PROTECT(Rf_allocVector(INTSXP, which_n));
  int* which_data = INTEGER(which);

  for (R_len_t i = 0; i < n; ++i, ++data) {
    int elt = *data;

    if (elt && elt != NA_LOGICAL) {
      *which_data = i + 1;
      ++which_data;
    }
  }

  UNPROTECT(1);
  return which;
}
