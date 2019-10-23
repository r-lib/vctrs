#include "vctrs.h"
#include "utils.h"
#include "dictionary.h"

SEXP vctrs_group_id(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out = INTEGER(out);

  R_len_t g = 1;

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(&d, i);
    R_len_t key = d.key[hash];

    if (key == DICT_EMPTY) {
      dict_put(&d, hash, i);
      p_out[i] = g;
      ++g;
    } else {
      p_out[i] = p_out[key];
    }
  }

  SEXP n_groups = PROTECT_N(Rf_ScalarInteger(d.used), &nprot);
  Rf_setAttrib(out, syms_n, n_groups);

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP new_group_rle(SEXP g, SEXP l, R_len_t n);

SEXP vctrs_group_rle(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  SEXP g = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_g = INTEGER(g);

  SEXP l = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_l = INTEGER(l);

  if (n == 0) {
    SEXP out = PROTECT_N(new_group_rle(g, l, 0), &nprot);
    UNPROTECT(nprot);
    return out;
  }

  // Integer vector that maps `hash` values to positions in `g`
  SEXP map = PROTECT_N(Rf_allocVector(INTSXP, d.size), &nprot);
  int* p_map = INTEGER(map);

  // Initialize first value
  int32_t hash = dict_hash_scalar(&d, 0);
  dict_put(&d, hash, 0);
  p_map[hash] = 0;
  *p_g = 1;
  *p_l = 1;

  int pos = 1;

  for (int i = 1; i < n; ++i) {
    if (equal_scalar(x, i - 1, x, i, true)) {
      ++(*p_l);
      continue;
    }

    ++p_l;
    *p_l = 1;

    // Check if we have seen this value before
    int32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
      p_map[hash] = pos;
      p_g[pos] = d.used;
    } else {
      p_g[pos] = p_g[p_map[hash]];
    }

    ++pos;
  }

  g = PROTECT_N(Rf_lengthgets(g, pos), &nprot);
  l = PROTECT_N(Rf_lengthgets(l, pos), &nprot);

  SEXP out = new_group_rle(g, l, d.used);

  UNPROTECT(nprot);
  return out;
}

static SEXP new_group_rle(SEXP g, SEXP l, R_len_t n) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(out, 0, g);
  SET_VECTOR_ELT(out, 1, l);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, strings_group);
  SET_STRING_ELT(names, 1, strings_length);
  Rf_setAttrib(out, R_NamesSymbol, names);

  SEXP n_groups = PROTECT(Rf_ScalarInteger(n));
  Rf_setAttrib(out, syms_n, n_groups);

  Rf_setAttrib(out, R_ClassSymbol, classes_vctrs_group_rle);

  UNPROTECT(3);
  return out;
}
