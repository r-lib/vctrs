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


// R interface -----------------------------------------------------------------

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

SEXP vctrs_count(SEXP x) {
  R_len_t size = vec_length(x);
  SEXP key = PROTECT(Rf_allocVector(INTSXP, size));
  int* pKey = INTEGER(key);
  for (R_len_t k = 0; k < size; ++k) {
    pKey[k] = -1;
  }

  R_len_t n = vec_length(x);
  SEXP val = PROTECT(Rf_allocVector(INTSXP, size));
  int* pVal = INTEGER(val);

  R_len_t n_unique = 0;

  for (int i = 0; i < n; ++i) {
    int32_t k = hash_table_find(key, size, x, i);

    if (pKey[k] == -1) {
      pKey[k] = i;
      pVal[k] = 0;
      n_unique++;
    }
    pVal[k]++;
  }

  SEXP out_idx = PROTECT(Rf_allocVector(INTSXP, n_unique));
  SEXP out_val = PROTECT(Rf_allocVector(INTSXP, n_unique));
  int* p_out_idx = INTEGER(out_idx);
  int* p_out_val = INTEGER(out_val);

  int i = 0;
  for (int k = 0; k < size; ++k) {
    if (pKey[k] == -1)
      continue;
    p_out_idx[i] = pKey[k] + 1;
    p_out_val[i] = pVal[k];
    i++;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, out_idx);
  SET_VECTOR_ELT(out, 1, out_val);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("idx"));
  SET_STRING_ELT(names, 1, Rf_mkChar("count"));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(6);
  return out;
}

