#include "vctrs.h"
#include "utils.h"
#include "dictionary.h"

// [[ register() ]]
SEXP vec_split_id(SEXP x) {
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

// [[ register() ]]
SEXP vec_split(SEXP x, SEXP by) {
  if (vec_size(x) != vec_size(by)) {
    Rf_errorcall(R_NilValue, "`x` and `by` must have the same size.");
  }

  SEXP out = PROTECT(vec_split_id(by));

  SEXP indices = VECTOR_ELT(out, 1);

  SEXP ptype = PROTECT(vec_type(x));

  SEXP val = PROTECT(vec_split_along(x, indices));
  init_list_of(val, ptype);

  SET_VECTOR_ELT(out, 1, val);

  SEXP names = PROTECT(Rf_getAttrib(out, R_NamesSymbol));
  SET_STRING_ELT(names, 1, strings_val);
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(4);
  return out;
}

// -----------------------------------------------------------------------------

struct vctrs_split_info {
  struct vctrs_proxy_info proxy_info;
  SEXP restore_size;
  int* p_restore_size;
  SEXP index;
  int* p_index;
  bool has_indices;
  SEXP elt;
  R_len_t out_size;
  SEXP out;
};

#define PROTECT_SPLIT_INFO(info, n) do {                  \
  PROTECT_PROXY_INFO(&(info)->proxy_info, n);             \
  PROTECT((info)->restore_size);                          \
  PROTECT((info)->index);                                 \
  PROTECT((info)->elt);                                   \
  PROTECT((info)->out);                                   \
  *n += 4;                                                \
} while (0)                                               \

struct vctrs_split_info init_split_info(SEXP x, SEXP indices) {
  int nprot = 0;

  struct vctrs_split_info info;

  struct vctrs_proxy_info proxy_info = vec_proxy_info(x);
  PROTECT_PROXY_INFO(&proxy_info, &nprot);
  info.proxy_info = proxy_info;

  info.restore_size = PROTECT_N(r_int(1), &nprot);
  info.p_restore_size = INTEGER(info.restore_size);

  info.index = PROTECT_N(r_int(0), &nprot);
  info.p_index = INTEGER(info.index);

  info.elt = R_NilValue;

  if (indices == R_NilValue) {
    info.out_size = vec_size(x);
    info.has_indices = false;
  } else {
    info.out_size = vec_size(indices);
    info.has_indices = true;
  }

  info.out = PROTECT_N(Rf_allocVector(VECSXP, info.out_size), &nprot);

  UNPROTECT(nprot);
  return info;
}

// -----------------------------------------------------------------------------

SEXP split_along(SEXP x, struct vctrs_split_info info, SEXP indices);
SEXP split_along_shaped(SEXP x, struct vctrs_split_info info, SEXP indices);
SEXP split_along_df(SEXP x, struct vctrs_split_info info, SEXP indices);
SEXP split_along_fallback(SEXP x, struct vctrs_split_info info, SEXP indices);
SEXP split_along_fallback_shaped(SEXP x, struct vctrs_split_info info, SEXP indices);

SEXP as_split_indices(SEXP indices, SEXP x);

SEXP vec_split_along_impl(SEXP x, struct vctrs_split_info info, SEXP indices);


// [[ register() ]]
SEXP vctrs_split_along(SEXP x, SEXP indices) {
  indices = PROTECT(as_split_indices(indices, x));

  UNPROTECT(1);
  return vec_split_along(x, indices);
}

// [[ include("vctrs.h") ]]
SEXP vec_split_along(SEXP x, SEXP indices) {
  int nprot = 0;

  struct vctrs_split_info info = init_split_info(x, indices);
  PROTECT_SPLIT_INFO(&info, &nprot);

  SEXP out = PROTECT_N(vec_split_along_impl(x, info, indices), &nprot);

  UNPROTECT(nprot);
  return out;
}

SEXP vec_split_along_impl(SEXP x, struct vctrs_split_info info, SEXP indices) {
  struct vctrs_proxy_info proxy_info = info.proxy_info;

  // Fallback to `[` if the class doesn't implement a proxy. This is
  // to be maximally compatible with existing classes.
  if (OBJECT(x) && proxy_info.proxy_method == R_NilValue) {
    if (proxy_info.type == vctrs_type_scalar) {
      Rf_errorcall(R_NilValue, "Can't slice a scalar");
    }

    if (has_dim(x)) {
      return split_along_fallback_shaped(x, info, indices);
    }

    return split_along_fallback(x, info, indices);
  }

  switch (proxy_info.type) {
  case vctrs_type_null: {
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
      return split_along_shaped(x, info, indices);
    }

    return split_along(x, info, indices);
  }
  case vctrs_type_dataframe: {
    return split_along_df(x, info, indices);
  }
  default:
    vec_assert(x, args_empty);
    Rf_error(
      "Internal error: Unexpected type `%s` for vector proxy in `vec_split_along()`",
      vec_type_as_str(proxy_info.type)
    );
  }
}

SEXP split_along(SEXP x, struct vctrs_split_info info, SEXP indices) {
  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  PROTECT_INDEX name_prot_idx;
  SEXP name = R_NilValue;
  PROTECT_WITH_INDEX(name, &name_prot_idx);

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_size(info.index);
    } else {
      ++(*info.p_index);
    }

    info.elt = PROTECT(vec_slice_base(info.proxy_info.type, info.proxy_info.proxy, info.index));

    if (names != R_NilValue) {
      name = slice_names(names, info.index);
      REPROTECT(name, name_prot_idx);
      r_poke_names(info.elt, name);
    }

    info.elt = vec_restore(info.elt, x, info.restore_size);

    SET_VECTOR_ELT(info.out, i, info.elt);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return info.out;
}

SEXP split_along_df2(SEXP x, struct vctrs_split_info info, SEXP indices) {
  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_size(info.index);
    } else {
      ++(*info.p_index);
    }

    info.elt = PROTECT(df_slice(info.proxy_info.proxy, info.index));

    info.elt = vec_restore(info.elt, x, info.restore_size);

    SET_VECTOR_ELT(info.out, i, info.elt);
    UNPROTECT(1);
  }

  return info.out;
}

SEXP split_along_df(SEXP x, struct vctrs_split_info info, SEXP indices) {
  int n_cols = Rf_length(x);

  SEXP col_names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  SEXP row_names = PROTECT(get_rownames(x));

  bool has_row_names = TYPEOF(row_names) == STRSXP;

  // Pre-load the `out` container with lists that will become data frames
  for (R_len_t i = 0; i < info.out_size; ++i) {
    info.elt = PROTECT(Rf_allocVector(VECSXP, n_cols));

    Rf_setAttrib(info.elt, R_NamesSymbol, col_names);

    if (has_row_names) {
      if (info.has_indices) {
        info.index = VECTOR_ELT(indices, i);
      } else {
        ++(*info.p_index);
      }

      Rf_setAttrib(info.elt, R_RowNamesSymbol, slice_rownames(row_names, info.index));
    }

    SET_VECTOR_ELT(info.out, i, info.elt);
    UNPROTECT(1);
  }

  // Split each column according to the indices, and then assign the results
  // into the appropriate data frame column in the `out` list
  for (int i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(info.proxy_info.proxy, i);
    SEXP split = PROTECT(vec_split_along(col, indices));

    for (int j = 0; j < info.out_size; ++j) {
      info.elt = VECTOR_ELT(info.out, j);
      SET_VECTOR_ELT(info.elt, i, VECTOR_ELT(split, j));
    }

    UNPROTECT(1);
  }

  // Restore each data frame
  for (int i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      *info.p_restore_size = vec_size(VECTOR_ELT(indices, i));
    }

    info.elt = VECTOR_ELT(info.out, i);
    info.elt = vec_restore(info.elt, x, info.restore_size);

    SET_VECTOR_ELT(info.out, i, info.elt);
  }

  UNPROTECT(2);
  return info.out;
}

SEXP split_along_shaped(SEXP x, struct vctrs_split_info info, SEXP indices) {
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

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_size(info.index);
    } else {
      ++(*info.p_index);
    }

    info.elt = PROTECT(vec_slice_shaped(info.proxy_info.type, info.proxy_info.proxy, info.index));

    if (dim_names != R_NilValue) {
      if (row_names != R_NilValue) {
        new_dim_names = Rf_shallow_duplicate(dim_names);
        REPROTECT(new_dim_names, new_dim_names_prot_idx);

        new_row_names = slice_names(row_names, info.index);
        REPROTECT(new_row_names, new_row_names_prot_idx);

        SET_VECTOR_ELT(new_dim_names, 0, new_row_names);

        Rf_setAttrib(info.elt, R_DimNamesSymbol, new_dim_names);
      } else {
        Rf_setAttrib(info.elt, R_DimNamesSymbol, dim_names);
      }
    }

    info.elt = vec_restore(info.elt, x, info.restore_size);

    SET_VECTOR_ELT(info.out, i, info.elt);
    UNPROTECT(1);
  }

  UNPROTECT(3);
  return info.out;
}

SEXP split_along_fallback(SEXP x, struct vctrs_split_info info, SEXP indices) {
  // Construct call with symbols, not values, for performance
  SEXP call = PROTECT(Rf_lang3(syms_bracket, syms_x, syms_i));

  SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 2));
  Rf_defineVar(syms_bracket, fns_bracket, env);
  Rf_defineVar(syms_x, x, env);
  Rf_defineVar(syms_i, info.index, env);

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_size(info.index);

      // Must redefine after altering
      Rf_defineVar(syms_i, info.index, env);
    } else {
      ++(*info.p_index);
    }

    info.elt = PROTECT(Rf_eval(call, env));

    // Restore attributes only if `[` fallback doesn't
    if (ATTRIB(info.elt) == R_NilValue) {
      info.elt = vec_restore(info.elt, x, info.restore_size);
    }

    SET_VECTOR_ELT(info.out, i, info.elt);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return info.out;
}

SEXP split_along_fallback_shaped(SEXP x, struct vctrs_split_info info, SEXP indices) {
  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
    } else {
      ++(*info.p_index);
    }

    // `vec_slice_fallback()` will also `vec_restore()` for us
    info.elt = PROTECT(vec_slice_fallback(x, info.index));

    SET_VECTOR_ELT(info.out, i, info.elt);
    UNPROTECT(1);
  }

  return info.out;
}

SEXP as_split_indices(SEXP indices, SEXP x) {
  if (indices == R_NilValue) {
    return indices;
  }

  if (TYPEOF(indices) != VECSXP) {
    Rf_errorcall(R_NilValue, "`indices` must be a list of index values, or `NULL`.");
  }

  SEXP index;
  indices = PROTECT(r_maybe_duplicate(indices));

  R_len_t size_indices = vec_size(indices);
  R_len_t size_x = vec_size(x);
  SEXP names = PROTECT(vec_names(x));

  for (int i = 0; i < size_indices; ++i) {
    index = VECTOR_ELT(indices, i);
    SET_VECTOR_ELT(indices, i, vec_as_index(index, size_x, names));
  }

  UNPROTECT(2);
  return indices;
}
