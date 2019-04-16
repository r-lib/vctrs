#include "vctrs.h"
#include "utils.h"

// Initialised at load time
static SEXP unspecified_class = NULL;
SEXP vctrs_shared_empty_uns = NULL;


// [[ include("vctrs.h") ]]
SEXP vec_unspecified(R_len_t n) {
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  r_lgl_fill(out, NA_LOGICAL);
  Rf_setAttrib(out, R_ClassSymbol, unspecified_class);

  UNPROTECT(1);
  return out;
}

// [[ register ]]
SEXP vctrs_unspecified(SEXP n) {
  if (Rf_length(n) != 1) {
    Rf_errorcall(R_NilValue, "`n` must be a single number");
  }
  if (TYPEOF(n) != INTSXP) {
    n = vec_cast(n, vctrs_shared_empty_int);
  }
  int len = INTEGER(n)[0];
  return vec_unspecified(len);
}

// [[ include("vctrs.h") ]]
bool vec_is_unspecified(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    return false;
  }

  if (ATTRIB(x) != R_NilValue) {
    if (Rf_inherits(x, "vctrs_unspecified")) {
      return true;
    }
    if (OBJECT(x)) {
      return false;
    }
    if (has_dim(x)) {
      return false;
    }
  }

  R_len_t n = Rf_length(x);
  if (n == 0) {
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

// [[ register ]]
SEXP vctrs_is_unspecified(SEXP x) {
  return Rf_ScalarLogical(vec_is_unspecified(x));
}


void vctrs_init_unspecified(SEXP ns) {
  unspecified_class = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(unspecified_class);
  SET_STRING_ELT(unspecified_class, 0, Rf_mkChar("vctrs_unspecified"));

  vctrs_shared_empty_uns = vec_unspecified(0);
  R_PreserveObject(vctrs_shared_empty_uns);
  MARK_NOT_MUTABLE(vctrs_shared_empty_uns);
}
