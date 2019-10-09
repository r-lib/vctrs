#include "vctrs.h"
#include "utils.h"
#include "dictionary.h"

SEXP vctrs_split_id(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  SEXP proxy = PROTECT_N(vec_proxy_equal(x), &nprot);
  proxy = PROTECT_N(obj_maybe_translate_encoding(proxy, n), &nprot);

  dictionary d;
  dict_init(&d, proxy);
  PROTECT_DICT(&d, &nprot);

  // Tracks the order in which keys are seen
  SEXP tracker = PROTECT_N(Rf_allocVector(INTSXP, d.size), &nprot);
  int* p_tracker = INTEGER(tracker);

  // Collects the counts of each key
  SEXP count = PROTECT_N(Rf_allocVector(INTSXP, d.size), &nprot);
  int* p_count = INTEGER(count);

  // Tells us which element of the index list x[i] goes in
  SEXP out_pos = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out_pos = INTEGER(out_pos);

  // Fill dictionary, out_pos, and count
  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      p_tracker[hash] = d.used;
      dict_put(&d, hash, i);
      p_count[hash] = 0;
    }

    p_out_pos[i] = p_tracker[hash];
    p_count[hash]++;
  }

  // Track the first position of each key in `x`
  SEXP key_id = PROTECT_N(Rf_allocVector(INTSXP, d.used), &nprot);
  int* p_key_id = INTEGER(key_id);

  SEXP out_id = PROTECT_N(Rf_allocVector(VECSXP, d.used), &nprot);
  init_list_of(out_id, vctrs_shared_empty_int);

  SEXP counters = PROTECT_N(Rf_allocVector(INTSXP, d.used), &nprot);
  int* p_counters = INTEGER(counters);
  memset(p_counters, 0, d.used * sizeof(int));

  // Set up empty index container
  for (int hash = 0; hash < d.size; ++hash) {
    if (d.key[hash] == DICT_EMPTY) {
      continue;
    }

    SET_VECTOR_ELT(out_id, p_tracker[hash], Rf_allocVector(INTSXP, p_count[hash]));
  }

  // Fill index container and key locations
  for (int i = 0; i < n; ++i) {
    int j = p_out_pos[i];
    int hash = p_counters[j];

    if (hash == 0) {
      p_key_id[j] = i + 1;
    }

    INTEGER(VECTOR_ELT(out_id, j))[hash] = i + 1;
    p_counters[j] = hash + 1;
  }

  SEXP out_key = PROTECT_N(vec_slice(x, key_id), &nprot);

  // Construct output data frame
  SEXP out = PROTECT_N(Rf_allocVector(VECSXP, 2), &nprot);
  SET_VECTOR_ELT(out, 0, out_key);
  SET_VECTOR_ELT(out, 1, out_id);

  SEXP names = PROTECT_N(Rf_allocVector(STRSXP, 2), &nprot);
  SET_STRING_ELT(names, 0, strings_key);
  SET_STRING_ELT(names, 1, strings_id);

  Rf_setAttrib(out, R_NamesSymbol, names);

  out = new_data_frame(out, d.used);

  UNPROTECT(nprot);
  return out;
}
