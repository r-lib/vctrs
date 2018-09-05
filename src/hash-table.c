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

// http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
int32_t ceil2(int32_t x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;
  return x;
}

// Hashtable object ------------------------------------------------------------

struct HASHTABLE {
  SEXP x;
  SEXP key;    // INTSXP
  int* p_key;
  SEXP val;
  uint32_t size;
  uint32_t used;
};

// Caller is responsible for PROTECTing x; and for calling hash_table_term
// Use val_t = NILSXP if need set-like behaviour, rather than dictionary
void hash_table_init(struct HASHTABLE* d, SEXP x, SEXPTYPE val_t) {
  d->x = x;

  // round up to power of 2
  // once dictionary is resizable we'll reduce this to a smaller number
  R_len_t size = ceil2(vec_length(x));

  d->key = PROTECT(Rf_allocVector(INTSXP, size));
  d->p_key = INTEGER(d->key);
  for (R_len_t i = 0; i < size; ++i) {
    d->p_key[i] = -1;
  }

  if (val_t == NILSXP) {
    d->val = R_NilValue;
  } else {
    d->val = PROTECT(Rf_allocVector(val_t, size));
  }

  d->size = size;
  d->used = 0;
}

void hash_table_term(struct HASHTABLE* d) {
  if (TYPEOF(d->val) == NILSXP) {
    UNPROTECT(1);
  } else {
    UNPROTECT(2);
  }
}

SEXP hash_table_contents_int(struct HASHTABLE* d) {
  SEXP key = PROTECT(Rf_allocVector(INTSXP, d->used));
  SEXP val = PROTECT(Rf_allocVector(INTSXP, d->used));
  int* p_out_key = INTEGER(key);
  int* p_out_val = INTEGER(val);

  int* p_val = INTEGER(d->val);

  int i = 0;
  for (int k = 0; k < d->size; ++k) {
    if (d->p_key[k] == -1)
      continue;

    p_out_key[i] = d->p_key[k] + 1;
    p_out_val[i] = p_val[k];
    i++;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, key);
  SET_VECTOR_ELT(out, 1, val);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("key"));
  SET_STRING_ELT(names, 1, Rf_mkChar("val"));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(4);
  return out;
}

uint32_t hash_table_find(struct HASHTABLE* d, SEXP x, R_len_t i) {
  uint32_t hv = hash_scalar(x, i);

  // quadratic probing: will try every slot if d->size is power of 2
  // http://research.cs.vt.edu/AVresearch/hashing/quadratic.php
  for (int k = 0; k < d->size; ++k) {
    uint32_t probe = (hv + k * (k + 1) / 2) % d->size;
    if (k > 1 && probe == hv) // circled back to start
      break;

    R_len_t idx = d->p_key[probe];
    if (idx == -1) // not used
      return probe;

    if (equal_scalar(d->x, idx, x, i)) // same value
      return probe;
  }

  Rf_errorcall(R_NilValue, "Hash table is full!");
}

void hash_table_put(struct HASHTABLE* d, uint32_t k, R_len_t i) {
  d->p_key[k] = i;
  d->used++;
}

// R interface -----------------------------------------------------------------

SEXP vctrs_duplicated(SEXP x) {
  struct HASHTABLE d;
  hash_table_init(&d, x, NILSXP);

  R_len_t n = vec_length(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* pOut = LOGICAL(out);

  for (int i = 0; i < n; ++i) {
    uint32_t k = hash_table_find(&d, x, i);

    if (d.p_key[k] == -1) {
      hash_table_put(&d, k, i);
      pOut[i] = false;
    } else {
      pOut[i] = true;
    }
  }

  hash_table_term(&d);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_id(SEXP x) {
  struct HASHTABLE d;
  hash_table_init(&d, x, NILSXP);

  R_len_t n = vec_length(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int* pOut = INTEGER(out);

  for (int i = 0; i < n; ++i) {
    uint32_t k = hash_table_find(&d, x, i);

    if (d.p_key[k] == -1) {
      hash_table_put(&d, k, i);
    }
    pOut[i] = d.p_key[k] + 1;
  }

  hash_table_term(&d);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_count(SEXP x) {
  struct HASHTABLE d;
  hash_table_init(&d, x, INTSXP);
  int* p_val = INTEGER(d.val);

  R_len_t n = Rf_length(x);
  for (int i = 0; i < n; ++i) {
    int32_t k = hash_table_find(&d, x, i);

    if (d.p_key[k] == -1) {
      hash_table_put(&d, k, i);
      p_val[k] = 0;
    }
    p_val[k]++;
  }

  SEXP out = PROTECT(hash_table_contents_int(&d));
  hash_table_term(&d);
  UNPROTECT(1);
  return out;
}

