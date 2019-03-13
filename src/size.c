#include "vctrs.h"
#include "utils.h"


static SEXP int_slice_index(SEXP i, SEXP x) {
  if (Rf_length(i) == 1 && *INTEGER(i) == 0) {
    return vctrs_shared_empty_int;
  } else {
    return i;
  }
}

static SEXP lgl_slice_index(SEXP i, SEXP x) {
  R_len_t n = Rf_length(i);

  if (n == Rf_length(x)) {
    return r_lgl_which(i);
  }

  if (n == 1) {
    if (*LOGICAL(i)) {
      return vctrs_shared_true;
    } else {
      return vctrs_shared_empty_int;
    }
  }

  Rf_errorcall(R_NilValue, "Logical indices must have length 1 "
               "or be as long as the indexed vector.\n"
               "Incompatible lengths: %d, %d",
               n, Rf_length(x));
}

static SEXP chr_slice_index(SEXP i, SEXP x) {
  SEXP nms = Rf_getAttrib(x, R_NamesSymbol);

  if (nms == R_NilValue) {
    Rf_errorcall(R_NilValue, "Can't use character to index an unnamed vector.");
  }

  return Rf_match(nms, i, NA_INTEGER);
}

// Should we check for NA in `i`?
SEXP vctrs_slice_index(SEXP i, SEXP x) {
  switch (TYPEOF(i)) {
  case INTSXP: return int_slice_index(i, x);
  case LGLSXP: return lgl_slice_index(i, x);
  case STRSXP: return chr_slice_index(i, x);

  // Do we really want to forbid numeric indices here (> 2^31)?
  default: Rf_errorcall(R_NilValue, "`i` must be an integer.");
  }
}
