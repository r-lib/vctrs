#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdbool.h>
#include <stdio.h>
#include "hash.h"

#define EMPTY -1


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
  SEXP x;
  int32_t* key;
  SEXP key_s;
  int key_protect;
  uint32_t size;
  uint32_t used;
};
typedef struct dictionary dictionary;

// Caller is responsible for PROTECTing x
void dict_init(dictionary* d, SEXP x) {
  d->x = x;

  // assume worst case, that every value is distinct, aiming for a load factor
  // of at most 77%. We round up to power of 2 to ensure quadratic probing
  // strategy works. Once the dictionary is resizable we'll reduce this to a
  // smaller number.
  R_len_t size = ceil2(vec_length(x) / 0.77);
  // Rprintf("size: %i\n", size);

  d->key_s = Rf_allocVector(INTSXP, size);
  PROTECT_WITH_INDEX(d->key_s, &d->key_protect);
  d->key = INTEGER(d->key_s);
  memset(d->key, -1, size * sizeof(int));

  d->size = size;
  d->used = 0;
}

void dict_free(dictionary* d) {
  UNPROTECT(1);
}

uint32_t dict_find(dictionary* d, SEXP y, R_len_t i) {
  uint32_t hv = hash_scalar(y, i);
  // Rprintf("i: %i hash: %i\n", i, hv);

  // quadratic probing: will try every slot if d->size is power of 2
  // http://research.cs.vt.edu/AVresearch/hashing/quadratic.php
  for (int k = 0; k < d->size; ++k) {
    uint32_t probe = (hv + k * (k + 1) / 2) % d->size;
    // Rprintf("Probe: %i\n", probe);
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
  dict_init(&d, x);

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

  UNPROTECT(1);
  dict_free(&d);
  return out;
}

SEXP vctrs_duplicated_any(SEXP x) {
  dictionary d;
  dict_init(&d, x);

  bool out = false;
  R_len_t n = vec_length(x);

  for (int i = 0; i < n; ++i) {
    uint32_t k = dict_find(&d, x, i);

    if (d.key[k] == EMPTY) {
      dict_put(&d, k, i);
    } else {
      out = true;
      break;
    }
  }

  dict_free(&d);
  return Rf_ScalarLogical(out);
}

SEXP vctrs_n_distinct(SEXP x) {
  dictionary d;
  dict_init(&d, x);

  R_len_t n = vec_length(x);
  for (int i = 0; i < n; ++i) {
    uint32_t k = dict_find(&d, x, i);

    if (d.key[k] == EMPTY)
      dict_put(&d, k, i);
  }

  dict_free(&d);
  return Rf_ScalarInteger(d.used);
}

SEXP vctrs_id(SEXP x) {
  dictionary d;
  dict_init(&d, x);

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

  UNPROTECT(1);
  dict_free(&d);
  return out;
}

SEXP vctrs_match(SEXP needles, SEXP haystack) {
  dictionary d;
  dict_init(&d, haystack);

  // Load dictionary with haystack
  R_len_t n = vec_length(haystack);
  for (int i = 0; i < n; ++i) {
    uint32_t k = dict_find(&d, haystack, i);

    if (d.key[k] == EMPTY) {
      dict_put(&d, k, i);
    }
  }

  // Locate needles
  R_len_t n_needle = vec_length(needles);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n_needle));
  int* p_out = INTEGER(out);

  for (int i = 0; i < n; ++i) {
    uint32_t k = dict_find(&d, needles, i);
    if (d.key[k] == EMPTY) {
      p_out[i] = NA_INTEGER;
    } else {
      p_out[i] = d.key[k] + 1;
    }
  }
  UNPROTECT(1);
  dict_free(&d);
  return out;
}

SEXP vctrs_count(SEXP x) {
  dictionary d;
  dict_init(&d, x);

  SEXP val = PROTECT(Rf_allocVector(INTSXP, d.size));
  int* p_val = INTEGER(val);

  R_len_t n = vec_length(x);
  for (int i = 0; i < n; ++i) {
    int32_t k = dict_find(&d, x, i);

    if (d.key[k] == EMPTY) {
      dict_put(&d, k, i);
      p_val[k] = 0;
    }
    p_val[k]++;
  }

  // Create output
  SEXP out_key = PROTECT(Rf_allocVector(INTSXP, d.used));
  SEXP out_val = PROTECT(Rf_allocVector(INTSXP, d.used));
  int* p_out_key = INTEGER(out_key);
  int* p_out_val = INTEGER(out_val);

  int i = 0;
  for (int k = 0; k < d.size; ++k) {
    if (d.key[k] == EMPTY)
      continue;

    p_out_key[i] = d.key[k] + 1;
    p_out_val[i] = p_val[k];
    i++;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, out_key);
  SET_VECTOR_ELT(out, 1, out_val);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("key"));
  SET_STRING_ELT(names, 1, Rf_mkChar("val"));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(5);
  dict_free(&d);
  return out;
}
