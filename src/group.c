#include <rlang.h>
#include "vctrs.h"
#include "dictionary.h"
#include "translate.h"
#include "type-data-frame.h"
#include "utils.h"

// [[ register() ]]
SEXP vctrs_group_id(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out = INTEGER(out);

  R_len_t g = 1;

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);
    R_len_t key = d->key[hash];

    if (key == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_out[i] = g;
      ++g;
    } else {
      p_out[i] = p_out[key];
    }
  }

  SEXP n_groups = PROTECT_N(Rf_ScalarInteger(d->used), &nprot);
  Rf_setAttrib(out, syms_n, n_groups);

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP new_group_rle(SEXP g, SEXP l, R_len_t n);

// [[ register() ]]
SEXP vctrs_group_rle(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  const void* p_vec = d->p_poly_vec->p_vec;

  SEXP g = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_g = INTEGER(g);

  SEXP l = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_l = INTEGER(l);

  if (n == 0) {
    SEXP out = PROTECT_N(new_group_rle(g, l, 0), &nprot);
    UNPROTECT(nprot);
    return out;
  }

  // Integer vector that maps `hash` values to locations in `g`
  SEXP map = PROTECT_N(Rf_allocVector(INTSXP, d->size), &nprot);
  int* p_map = INTEGER(map);

  // Initialize first value
  uint32_t hash = dict_hash_scalar(d, 0);
  dict_put(d, hash, 0);
  p_map[hash] = 0;
  *p_g = 1;
  *p_l = 1;

  int loc = 1;

  for (int i = 1; i < n; ++i) {
    if (d->p_equal_na_equal(p_vec, i - 1, p_vec, i)) {
      ++(*p_l);
      continue;
    }

    ++p_l;
    *p_l = 1;

    // Check if we have seen this value before
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_map[hash] = loc;
      p_g[loc] = d->used;
    } else {
      p_g[loc] = p_g[p_map[hash]];
    }

    ++loc;
  }

  g = PROTECT_N(Rf_lengthgets(g, loc), &nprot);
  l = PROTECT_N(Rf_lengthgets(l, loc), &nprot);

  SEXP out = new_group_rle(g, l, d->used);

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

// -----------------------------------------------------------------------------

// [[ include("vctrs.h"); register() ]]
SEXP vec_group_loc(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  SEXP proxy = PROTECT_N(vec_proxy_equal(x), &nprot);
  proxy = PROTECT_N(vec_normalize_encoding(proxy), &nprot);

  struct dictionary* d = new_dictionary(proxy);
  PROTECT_DICT(d, &nprot);

  SEXP groups = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_groups = INTEGER(groups);

  R_len_t g = 0;

  // Identify groups, this is essentially `vec_group_id()`
  for (int i = 0; i < n; ++i) {
    const uint32_t hash = dict_hash_scalar(d, i);
    const R_len_t key = d->key[hash];

    if (key == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_groups[i] = g;
      ++g;
    } else {
      p_groups[i] = p_groups[key];
    }
  }

  const int n_groups = d->used;

  // Location of first occurence of each group in `x`
  SEXP key_loc = PROTECT_N(Rf_allocVector(INTSXP, n_groups), &nprot);
  int* p_key_loc = INTEGER(key_loc);
  int key_loc_current = 0;

  // Count of the number of elements in each group
  SEXP counts = PROTECT_N(Rf_allocVector(INTSXP, n_groups), &nprot);
  int* p_counts = INTEGER(counts);
  memset(p_counts, 0, n_groups * sizeof(int));

  for (int i = 0; i < n; ++i) {
    const int group = p_groups[i];

    if (group == key_loc_current) {
      p_key_loc[key_loc_current] = i + 1;
      ++key_loc_current;
    }

    ++p_counts[group];
  }

  SEXP out_loc = PROTECT_N(Rf_allocVector(VECSXP, n_groups), &nprot);

  // Direct pointer to the location vectors we store in `out_loc`
  int** p_elt_loc = (int**) R_alloc(n_groups, sizeof(int*));

  // Initialize `out_loc` to a list of integers with sizes corresponding
  // to the number of elements in that group
  for (int i = 0; i < n_groups; ++i) {
    SEXP elt_loc = Rf_allocVector(INTSXP, p_counts[i]);
    p_elt_loc[i] = INTEGER(elt_loc);
    SET_VECTOR_ELT(out_loc, i, elt_loc);
  }

  // The current location we are updating, each group has its own counter
  SEXP locations = PROTECT_N(Rf_allocVector(INTSXP, n_groups), &nprot);
  int* p_locations = INTEGER(locations);
  memset(p_locations, 0, n_groups * sizeof(int));

  // Fill in the location values for each group
  for (int i = 0; i < n; ++i) {
    const int group = p_groups[i];
    const int location = p_locations[group];
    p_elt_loc[group][location] = i + 1;
    ++p_locations[group];
  }

  SEXP out_key = PROTECT_N(vec_slice(x, key_loc), &nprot);

  // Construct output data frame
  SEXP out = PROTECT_N(Rf_allocVector(VECSXP, 2), &nprot);
  SET_VECTOR_ELT(out, 0, out_key);
  SET_VECTOR_ELT(out, 1, out_loc);

  SEXP names = PROTECT_N(Rf_allocVector(STRSXP, 2), &nprot);
  SET_STRING_ELT(names, 0, strings_key);
  SET_STRING_ELT(names, 1, strings_loc);

  Rf_setAttrib(out, R_NamesSymbol, names);

  out = new_data_frame(out, n_groups);

  UNPROTECT(nprot);
  return out;
}
