#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdbool.h>
#include "hash.h"

bool equal_scalar(SEXP x, int i, SEXP y, int j) {
  if (TYPEOF(x) != TYPEOF(y))
    return false;

  switch(TYPEOF(x)) {
  case LGLSXP:
    return LOGICAL(x)[i] == LOGICAL(y)[j];
  case INTSXP:
    return INTEGER(x)[i] == INTEGER(y)[j];
  case REALSXP: {
    double xi = REAL(x)[i], yj = REAL(y)[j];
    if (R_IsNA(xi)) return R_IsNA(yj);
    if (R_IsNaN(xi)) return R_IsNaN(yj);
    return xi == yj;
  }
  case STRSXP:
    // Ignoring encoding for now
    return STRING_ELT(x, i) == STRING_ELT(y, j);
  case VECSXP:
    // Need to add support for data frame
    return R_compute_identical(VECTOR_ELT(x, i), VECTOR_ELT(y, j), 16);
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

uint32_t hash_table_find(SEXP key, R_len_t n, SEXP x, R_len_t i) {
  uint32_t hv = hash_scalar(x, i);

  // quadratic probing
  // https://github.com/attractivechaos/klib/blob/master/khash.h#L51-L53
  for (int k = 0; k < n; ++k) {
    uint32_t probe = (hv + (k * k + k) / 2) % n;
    if (k > 1 && probe == hv) // circled back to start
      break;

    R_len_t idx = INTEGER(key)[probe];
    if (idx == -1) // not used
      return probe;

    if (equal_scalar(x, i, x, idx)) // same value
      return probe;
  }

  Rf_errorcall(R_NilValue, "Hash table is full!");
}

SEXP vctrs_duplicated(SEXP x) {
  R_len_t size = vec_length(x);
  SEXP key = PROTECT(Rf_allocVector(INTSXP, size));
  int* pKey = INTEGER(key);
  for (R_len_t i = 0; i < size; ++i) {
    pKey[i] = -1;
  }

  R_len_t n = vec_length(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pOut = LOGICAL(out);

  for (int i = 0; i < n; ++i) {
    uint32_t loc = hash_table_find(key, size, x, i);

    if (pKey[loc] == -1) {
      pOut[i] = false;
      pKey[loc] = i;
    } else {
      pOut[i] = true;
    }
  }

  UNPROTECT(2);
  return out;
}

// R interface -----------------------------------------------------------------

