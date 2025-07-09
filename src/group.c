#include "vctrs.h"
#include "type-data-frame.h"

#include "decl/group-decl.h"

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

  vctrs_group_id_loop(d, n, p_out);

  SEXP n_groups = PROTECT_N(Rf_ScalarInteger(d->used), &nprot);
  Rf_setAttrib(out, syms_n, n_groups);

  UNPROTECT(nprot);
  return out;
}

#define VCTRS_GROUP_ID_LOOP(DICT_HASH_SCALAR)      \
do {                                               \
  int g = 1;                                       \
                                                   \
  for (int i = 0; i < n; ++i) {                    \
    uint32_t hash = DICT_HASH_SCALAR(d, i);        \
    R_len_t key = d->key[hash];                    \
                                                   \
    if (key == DICT_EMPTY) {                       \
      dict_put(d, hash, i);                        \
      p_out[i] = g;                                \
      ++g;                                         \
    } else {                                       \
      p_out[i] = p_out[key];                       \
    }                                              \
  }                                                \
}                                                  \
while (0)

static inline
void vctrs_group_id_loop(struct dictionary* d, R_len_t n, int* p_out) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VCTRS_GROUP_ID_LOOP(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: VCTRS_GROUP_ID_LOOP(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: VCTRS_GROUP_ID_LOOP(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: VCTRS_GROUP_ID_LOOP(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: VCTRS_GROUP_ID_LOOP(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: VCTRS_GROUP_ID_LOOP(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: VCTRS_GROUP_ID_LOOP(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: VCTRS_GROUP_ID_LOOP(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: VCTRS_GROUP_ID_LOOP(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("vctrs_group_id_loop", d->p_poly_vec->type);
  }
}

#undef VCTRS_GROUP_ID_LOOP

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_group_rle(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP g = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_g = INTEGER(g);

  SEXP l = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_l = INTEGER(l);

  if (n == 0) {
    SEXP out = PROTECT_N(new_group_rle(g, l, 0), &nprot);
    UNPROTECT(nprot);
    return out;
  }

  const R_len_t size = vctrs_group_rle_loop(d, n, p_g, p_l);

  g = PROTECT_N(Rf_lengthgets(g, size), &nprot);
  l = PROTECT_N(Rf_lengthgets(l, size), &nprot);

  SEXP out = new_group_rle(g, l, d->used);

  UNPROTECT(nprot);
  return out;
}

#define VCTRS_GROUP_RLE_LOOP(DICT_HASH_SCALAR, P_EQUAL_NA_EQUAL)      \
do {                                                                  \
  R_len_t loc = 0;                                                    \
  const void* p_vec = d->p_poly_vec->p_vec;                           \
                                                                      \
  /* Integer vector that maps `hash` values to locations in `g` */    \
  SEXP map = PROTECT(Rf_allocVector(INTSXP, d->size));                \
  int* p_map = INTEGER(map);                                          \
                                                                      \
  /* Initialize first value */                                        \
  uint32_t hash = DICT_HASH_SCALAR(d, 0);                             \
  dict_put(d, hash, 0);                                               \
  p_map[hash] = 0;                                                    \
  *p_g = 1;                                                           \
  *p_l = 1;                                                           \
  ++loc;                                                              \
                                                                      \
  for (R_len_t i = 1; i < n; ++i) {                                   \
    if (P_EQUAL_NA_EQUAL(p_vec, i - 1, p_vec, i)) {                   \
      ++(*p_l);                                                       \
      continue;                                                       \
    }                                                                 \
                                                                      \
    ++p_l;                                                            \
    *p_l = 1;                                                         \
                                                                      \
    /* Check if we have seen this value before */                     \
    uint32_t hash = DICT_HASH_SCALAR(d, i);                           \
                                                                      \
    if (d->key[hash] == DICT_EMPTY) {                                 \
      dict_put(d, hash, i);                                           \
      p_map[hash] = loc;                                              \
      p_g[loc] = d->used;                                             \
    } else {                                                          \
      p_g[loc] = p_g[p_map[hash]];                                    \
    }                                                                 \
                                                                      \
    ++loc;                                                            \
  }                                                                   \
                                                                      \
  UNPROTECT(1);                                                       \
  return loc;                                                         \
}                                                                     \
while (0)

static inline
R_len_t vctrs_group_rle_loop(struct dictionary* d, R_len_t n, int* p_g, int* p_l) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VCTRS_GROUP_RLE_LOOP(nil_dict_hash_scalar, p_nil_equal_na_equal); break;
  case VCTRS_TYPE_logical: VCTRS_GROUP_RLE_LOOP(lgl_dict_hash_scalar, p_lgl_equal_na_equal); break;
  case VCTRS_TYPE_integer: VCTRS_GROUP_RLE_LOOP(int_dict_hash_scalar, p_int_equal_na_equal); break;
  case VCTRS_TYPE_double: VCTRS_GROUP_RLE_LOOP(dbl_dict_hash_scalar, p_dbl_equal_na_equal); break;
  case VCTRS_TYPE_complex: VCTRS_GROUP_RLE_LOOP(cpl_dict_hash_scalar, p_cpl_equal_na_equal); break;
  case VCTRS_TYPE_character: VCTRS_GROUP_RLE_LOOP(chr_dict_hash_scalar, p_chr_equal_na_equal); break;
  case VCTRS_TYPE_raw: VCTRS_GROUP_RLE_LOOP(raw_dict_hash_scalar, p_raw_equal_na_equal); break;
  case VCTRS_TYPE_list: VCTRS_GROUP_RLE_LOOP(list_dict_hash_scalar, p_list_equal_na_equal); break;
  case VCTRS_TYPE_dataframe: VCTRS_GROUP_RLE_LOOP(df_dict_hash_scalar, p_df_equal_na_equal); break;
  default: stop_unimplemented_vctrs_type("vctrs_group_rle_loop", d->p_poly_vec->type);
  }
}

#undef VCTRS_GROUP_RLE_LOOP

static inline
SEXP new_group_rle(SEXP g, SEXP l, R_len_t n) {
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

  // Identify groups
  vec_group_loc_loop(d, n, p_groups);

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

// This is essentially `vec_group_id()`
#define VEC_GROUP_LOC_LOOP(DICT_HASH_SCALAR)      \
do {                                              \
  int g = 0;                                      \
                                                  \
  for (int i = 0; i < n; ++i) {                   \
    const uint32_t hash = DICT_HASH_SCALAR(d, i); \
    const R_len_t key = d->key[hash];             \
                                                  \
    if (key == DICT_EMPTY) {                      \
      dict_put(d, hash, i);                       \
      p_groups[i] = g;                            \
      ++g;                                        \
    } else {                                      \
      p_groups[i] = p_groups[key];                \
    }                                             \
  }                                               \
}                                                 \
while (0)

static inline
void vec_group_loc_loop(struct dictionary* d, R_len_t n, int* p_groups) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_GROUP_LOC_LOOP(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: VEC_GROUP_LOC_LOOP(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: VEC_GROUP_LOC_LOOP(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: VEC_GROUP_LOC_LOOP(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: VEC_GROUP_LOC_LOOP(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: VEC_GROUP_LOC_LOOP(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: VEC_GROUP_LOC_LOOP(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: VEC_GROUP_LOC_LOOP(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: VEC_GROUP_LOC_LOOP(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("vec_group_loc_loop", d->p_poly_vec->type);
  }
}

#undef VEC_GROUP_LOC_LOOP
