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

// -----------------------------------------------------------------------------

SEXP split_along(SEXP x, struct vctrs_proxy_info info);
SEXP split_along_shaped(SEXP x, struct vctrs_proxy_info info);
SEXP split_along_df(SEXP x, struct vctrs_proxy_info info);

SEXP split_along_fallback(SEXP x);
SEXP split_along_fallback_shaped(SEXP x);

// Used in `as_df_row()` to turn `x` into a list like:
// list(vec_slice(x, 1L), vec_slice(x, 2L), ...)
// but in a more efficient way

// [[ include("vctrs.h"); register() ]]
SEXP vec_split_along(SEXP x) {
  int nprot = 0;

  struct vctrs_proxy_info info = vec_proxy_info(x);
  PROTECT_PROXY_INFO(&info, &nprot);

  // Fallback to `[` if the class doesn't implement a proxy. This is
  // to be maximally compatible with existing classes.
  if (OBJECT(x) && info.proxy_method == R_NilValue) {
    if (info.type == vctrs_type_scalar) {
      Rf_errorcall(R_NilValue, "Can't slice a scalar");
    }

    if (has_dim(x)) {
      UNPROTECT(nprot);
      return split_along_fallback_shaped(x);
    }

    UNPROTECT(nprot);
    return split_along_fallback(x);
  }

  switch (info.type) {
  case vctrs_type_null: {
    UNPROTECT(nprot);
    return R_NilValue;
  }
  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list: {
    if (has_dim(x)) {
      UNPROTECT(nprot);
      return split_along_shaped(x, info);
    }

    UNPROTECT(nprot);
    return split_along(x, info);
  }

  case vctrs_type_dataframe: {
    UNPROTECT(nprot);
    return split_along_df(x, info);
  }
  default:
    vec_assert(x, args_empty);
    Rf_error("Internal error: Unexpected type `%s` for vector proxy in `vec_split_along()`",
             vec_type_as_str(info.type));
  }
}

SEXP split_along(SEXP x, struct vctrs_proxy_info info) {
  R_len_t size = vec_size(x);

  SEXP restore_size = PROTECT(r_int(1));

  SEXP index = PROTECT(r_int(0));
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, size));

  SEXP data = info.proxy;

  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  PROTECT_INDEX name_prot_idx;
  SEXP name = R_NilValue;
  PROTECT_WITH_INDEX(name, &name_prot_idx);

  for (R_len_t i = 0; i < size; ++i) {
    ++(*p_index);

    elt = vec_slice_base(info.type, data, index);
    REPROTECT(elt, elt_prot_idx);

    if (names != R_NilValue) {
      name = slice_names(names, index);
      REPROTECT(name, name_prot_idx);
      r_poke_names(elt, name);
    }

    elt = vec_restore(elt, x, restore_size);

    SET_VECTOR_ELT(out, i, elt);
  }

  UNPROTECT(6);
  return out;
}

SEXP split_along_df(SEXP x, struct vctrs_proxy_info info) {
  R_len_t size = vec_size(x);

  SEXP restore_size = PROTECT(r_int(1));

  SEXP index = PROTECT(r_int(0));
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, size));

  SEXP data = info.proxy;

  for (R_len_t i = 0; i < size; ++i) {
    ++(*p_index);

    elt = df_slice(data, index);
    REPROTECT(elt, elt_prot_idx);

    elt = vec_restore(elt, x, restore_size);

    SET_VECTOR_ELT(out, i, elt);
  }

  UNPROTECT(4);
  return out;
}

SEXP split_along_shaped(SEXP x, struct vctrs_proxy_info info) {
  R_len_t size = vec_size(x);

  SEXP restore_size = PROTECT(r_int(1));

  SEXP index = PROTECT(r_int(0));
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, size));

  SEXP data = info.proxy;

  SEXP dim_names = PROTECT(Rf_getAttrib(x, R_DimNamesSymbol));

  SEXP row_names = R_NilValue;
  if (dim_names != R_NilValue) {
    row_names = VECTOR_ELT(dim_names, 0);
  }

  PROTECT_INDEX new_dim_names_prot_idx;
  SEXP new_dim_names = R_NilValue;
  PROTECT_WITH_INDEX(new_dim_names, &new_dim_names_prot_idx);

  PROTECT_INDEX new_row_names_prot_idx;
  SEXP new_row_names = R_NilValue;
  PROTECT_WITH_INDEX(new_row_names, &new_row_names_prot_idx);

  for (R_len_t i = 0; i < size; ++i) {
    ++(*p_index);

    elt = vec_slice_shaped(info.type, data, index);
    REPROTECT(elt, elt_prot_idx);

    if (dim_names != R_NilValue) {
      if (row_names != R_NilValue) {
        new_dim_names = Rf_shallow_duplicate(dim_names);
        REPROTECT(new_dim_names, new_dim_names_prot_idx);

        new_row_names = slice_names(row_names, index);
        REPROTECT(new_row_names, new_row_names_prot_idx);

        SET_VECTOR_ELT(new_dim_names, 0, new_row_names);

        Rf_setAttrib(elt, R_DimNamesSymbol, new_dim_names);
      } else {
        Rf_setAttrib(elt, R_DimNamesSymbol, dim_names);
      }
    }

    elt = vec_restore(elt, x, restore_size);

    SET_VECTOR_ELT(out, i, elt);
  }

  UNPROTECT(7);
  return out;
}

SEXP split_along_fallback(SEXP x) {
  R_len_t size = vec_size(x);

  SEXP restore_size = PROTECT(r_int(1));

  SEXP index = PROTECT(r_int(0));
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, size));

  // Construct call with symbols, not values, for performance
  SEXP call = PROTECT(Rf_lang3(syms_bracket, syms_x, syms_i));

  SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 2));
  Rf_defineVar(syms_bracket, fns_bracket, env);
  Rf_defineVar(syms_x, x, env);
  Rf_defineVar(syms_i, index, env);

  for (R_len_t i = 0; i < size; ++i) {
    ++(*p_index);

    elt = Rf_eval(call, env);
    REPROTECT(elt, elt_prot_idx);

    // Restore attributes only if `[` fallback doesn't
    if (ATTRIB(elt) == R_NilValue) {
      elt = vec_restore(elt, x, restore_size);
    }

    SET_VECTOR_ELT(out, i, elt);
  }

  UNPROTECT(6);
  return out;
}

SEXP split_along_fallback_shaped(SEXP x) {
  R_len_t size = vec_size(x);

  SEXP index = PROTECT(r_int(0));
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, size));

  for (R_len_t i = 0; i < size; ++i) {
    ++(*p_index);

    // `vec_slice_fallback()` will also `vec_restore()` for us
    elt = vec_slice_fallback(x, index);
    REPROTECT(elt, elt_prot_idx);

    SET_VECTOR_ELT(out, i, elt);
  }

  UNPROTECT(3);
  return out;
}
