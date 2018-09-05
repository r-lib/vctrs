#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdbool.h>
#include "hash.h"

#define EMPTY -1

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

// Dictonary object ------------------------------------------------------------

// The dictionary structure is a little peculiar since R has no notion of
// a scalar, so the `key`s are indexes into vector `x`. This means we can
// only store values from a single vector, but we can still lookup using
// another vector, provided that they're of the same type.

struct dictionary {
  SEXP x;      // must be a vector
  int32_t* key;
  SEXP val;
  uint32_t size;
  uint32_t used;
};
typedef struct dictionary dictionary;

// Caller is responsible for PROTECTing x; and for calling dict_term
// Use val_t = NILSXP if need set-like behaviour, rather than dictionary
void dict_init(dictionary* d, SEXP x, SEXPTYPE val_t) {
  d->x = x;

  // round up to power of 2
  // once dictionary is resizable we'll reduce this to a smaller number
  R_len_t size = ceil2(vec_length(x));

  d->key = (int32_t*) R_alloc(size, sizeof(int32_t));
  for (R_len_t i = 0; i < size; ++i) {
    d->key[i] = EMPTY;
  }

  if (val_t == NILSXP) {
    d->val = R_NilValue;
  } else {
    d->val = PROTECT(Rf_allocVector(val_t, size));
  }

  d->size = size;
  d->used = 0;
}

void dict_term(dictionary* d) {
  if (TYPEOF(d->val) != NILSXP) {
    UNPROTECT(1);
  }
}

SEXP dict_contents_int(dictionary* d) {
  SEXP key = PROTECT(Rf_allocVector(INTSXP, d->used));
  SEXP val = PROTECT(Rf_allocVector(INTSXP, d->used));
  int* p_out_key = INTEGER(key);
  int* p_out_val = INTEGER(val);

  int* p_val = INTEGER(d->val);

  int i = 0;
  for (int k = 0; k < d->size; ++k) {
    if (d->key[k] == EMPTY)
      continue;

    p_out_key[i] = d->key[k] + 1;
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

uint32_t dict_find(dictionary* d, SEXP y, R_len_t i) {
  uint32_t hv = hash_scalar(y, i);

  // quadratic probing: will try every slot if d->size is power of 2
  // http://research.cs.vt.edu/AVresearch/hashing/quadratic.php
  for (int k = 0; k < d->size; ++k) {
    uint32_t probe = (hv + k * (k + 1) / 2) % d->size;
    if (k > 1 && probe == hv) // circled back to start
      break;

    R_len_t idx = d->key[probe];
    if (idx == EMPTY) // not used
      return probe;

    if (equal_scalar(d->x, idx, y, i)) // same value
      return probe;
  }

  Rf_errorcall(R_NilValue, "Dictionary is full!");
}

void dict_put(dictionary* d, uint32_t k, R_len_t i) {
  d->key[k] = i;
  d->used++;
}

// R interface -----------------------------------------------------------------

SEXP vctrs_duplicated(SEXP x) {
  dictionary d;
  dict_init(&d, x, NILSXP);

  R_len_t n = vec_length(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_out = LOGICAL(out);

  for (int i = 0; i < n; ++i) {
    uint32_t k = dict_find(&d, x, i);

    if (d.key[k] == EMPTY) {
      dict_put(&d, k, i);
      p_out[i] = false;
    } else {
      p_out[i] = true;
    }
  }

  dict_term(&d);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_n_distinct(SEXP x) {
  dictionary d;
  dict_init(&d, x, NILSXP);

  R_len_t n = vec_length(x);
  for (int i = 0; i < n; ++i) {
    uint32_t k = dict_find(&d, x, i);

    if (d.key[k] == EMPTY)
      dict_put(&d, k, i);
  }

  dict_term(&d);
  return Rf_ScalarInteger(d.used);
}

SEXP vctrs_id(SEXP x) {
  dictionary d;
  dict_init(&d, x, NILSXP);

  R_len_t n = vec_length(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int* p_out = INTEGER(out);

  for (int i = 0; i < n; ++i) {
    uint32_t k = dict_find(&d, x, i);

    if (d.key[k] == EMPTY) {
      dict_put(&d, k, i);
    }
    p_out[i] = d.key[k] + 1;
  }

  dict_term(&d);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_count(SEXP x) {
  dictionary d;
  dict_init(&d, x, INTSXP);
  int* p_val = INTEGER(d.val);

  R_len_t n = Rf_length(x);
  for (int i = 0; i < n; ++i) {
    int32_t k = dict_find(&d, x, i);

    if (d.key[k] == EMPTY) {
      dict_put(&d, k, i);
      p_val[k] = 0;
    }
    p_val[k]++;
  }

  SEXP out = PROTECT(dict_contents_int(&d));
  dict_term(&d);
  UNPROTECT(1);
  return out;
}

