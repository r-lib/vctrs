#include "vctrs.h"
#include "utils.h"
#include "dictionary.h"

// [[ register() ]]
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

// [[ register() ]]
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

// [[ register() ]]
SEXP vec_group_pos(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  SEXP proxy = PROTECT_N(vec_proxy_equal(x), &nprot);
  proxy = PROTECT_N(obj_maybe_translate_encoding(proxy, n), &nprot);

  dictionary d;
  dict_init(&d, proxy);
  PROTECT_DICT(&d, &nprot);

  SEXP groups = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_groups = INTEGER(groups);

  R_len_t g = 0;

  // Identify groups, this is essentially `vec_group_id()`
  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(&d, i);
    R_len_t key = d.key[hash];

    if (key == DICT_EMPTY) {
      dict_put(&d, hash, i);
      p_groups[i] = g;
      ++g;
    } else {
      p_groups[i] = p_groups[key];
    }
  }

  int n_groups = d.used;

  // Position of first occurence of each group in `x`
  SEXP key_pos = PROTECT_N(Rf_allocVector(INTSXP, n_groups), &nprot);
  int* p_key_pos = INTEGER(key_pos);
  int key_pos_current = 0;

  // Count of the number of elements in each group
  SEXP counts = PROTECT_N(Rf_allocVector(INTSXP, n_groups), &nprot);
  int* p_counts = INTEGER(counts);
  memset(p_counts, 0, n_groups * sizeof(int));

  for (int i = 0; i < n; ++i) {
    int group = p_groups[i];

    if (group == key_pos_current) {
      p_key_pos[key_pos_current] = i + 1;
      key_pos_current++;
    }

    p_counts[group]++;
  }

  SEXP out_pos = PROTECT_N(Rf_allocVector(VECSXP, n_groups), &nprot);
  init_list_of(out_pos, vctrs_shared_empty_int);

  // Initialize `out_pos` to a list of integers with sizes corresponding
  // to the number of elements in that group
  for (int i = 0; i < n_groups; ++i) {
    SET_VECTOR_ELT(out_pos, i, Rf_allocVector(INTSXP, p_counts[i]));
  }

  // The current position we are updating, each group has its own counter
  SEXP positions = PROTECT_N(Rf_allocVector(INTSXP, n_groups), &nprot);
  int* p_positions = INTEGER(positions);
  memset(p_positions, 0, n_groups * sizeof(int));

  // Fill in the position values for each group
  for (int i = 0; i < n; ++i) {
    int group = p_groups[i];
    int position = p_positions[group];
    INTEGER(VECTOR_ELT(out_pos, group))[position] = i + 1;
    p_positions[group]++;
  }

  SEXP out_key = PROTECT_N(vec_slice(x, key_pos), &nprot);

  // Construct output data frame
  SEXP out = PROTECT_N(Rf_allocVector(VECSXP, 2), &nprot);
  SET_VECTOR_ELT(out, 0, out_key);
  SET_VECTOR_ELT(out, 1, out_pos);

  SEXP names = PROTECT_N(Rf_allocVector(STRSXP, 2), &nprot);
  SET_STRING_ELT(names, 0, strings_key);
  // TODO - Change to `strings_pos` when we change
  // `vec_split_id()` -> `vec_group_pos()`
  SET_STRING_ELT(names, 1, strings_id);

  Rf_setAttrib(out, R_NamesSymbol, names);

  out = new_data_frame(out, n_groups);

  UNPROTECT(nprot);
  return out;
}
