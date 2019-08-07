#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_slice_fallback = NULL;
SEXP fns_vec_slice_fallback = NULL;

// Defined below
SEXP vec_as_index(SEXP i, R_len_t n, SEXP names);
static SEXP slice_names(SEXP names, SEXP index);
static SEXP slice_rownames(SEXP names, SEXP index);

/**
 * This `vec_slice()` variant falls back to `[` with S3 objects.
 *
 * @param to The type to restore to.
 * @param dispatch When `true`, dispatches to `[` for compatibility
 *   with base R. When `false`, uses native implementations.
 */
SEXP vec_slice_impl(SEXP x, SEXP index);


static void stop_bad_index_length(R_len_t n, R_len_t i) {
  Rf_errorcall(R_NilValue,
               "Can't index beyond the end of a vector.\n"
               "The vector has length %d and you've tried to subset element %d.",
               n, i);
}

#define SLICE_INDEX(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)   \
  const CTYPE* data = CONST_DEREF(x);                             \
  R_len_t n = Rf_length(index);                                   \
  int* index_data = INTEGER(index);                               \
                                                                  \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                   \
  CTYPE* out_data = DEREF(out);                                   \
                                                                  \
  for (R_len_t i = 0; i < n; ++i, ++index_data, ++out_data) {     \
    int j = *index_data;                                          \
    *out_data = (j == NA_INTEGER) ? NA_VALUE : data[j - 1];       \
  }                                                               \
                                                                  \
  UNPROTECT(1);                                                   \
  return out

#define SLICE_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE) \
  const CTYPE* data = CONST_DEREF(x);                                 \
                                                                      \
  int* index_data = INTEGER(index);                                   \
  R_len_t j = index_data[0];                                          \
  R_len_t n = index_data[1];                                          \
                                                                      \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                       \
  CTYPE* out_data = DEREF(out);                                       \
                                                                      \
  CTYPE elt = (j == NA_INTEGER) ? NA_VALUE : data[j - 1];             \
                                                                      \
  for (R_len_t i = 0; i < n; ++i, ++out_data) {                       \
    *out_data = elt;                                                  \
  }                                                                   \
                                                                      \
  UNPROTECT(1);                                                       \
  return out

#define SLICE_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF)           \
  int* index_data = INTEGER(index);                                   \
  R_len_t start = index_data[0];                                      \
  R_len_t n = index_data[1];                                          \
  R_len_t step = index_data[2];                                       \
                                                                      \
  const CTYPE* data = CONST_DEREF(x) + start;                         \
                                                                      \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                       \
  CTYPE* out_data = DEREF(out);                                       \
                                                                      \
  for (int i = 0; i < n; ++i, ++out_data, data += step) {             \
    *out_data = *data;                                                \
  }                                                                   \
                                                                      \
  UNPROTECT(1);                                                       \
  return out

#define SLICE(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)          \
  if (is_compact_rep(index)) {                                     \
    SLICE_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE); \
  } else if (is_compact_seq(index)) {                              \
    SLICE_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF);           \
  } else {                                                         \
    SLICE_INDEX(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE);       \
  }

static SEXP lgl_slice(SEXP x, SEXP index) {
  SLICE(LGLSXP, int, LOGICAL, LOGICAL_RO, NA_LOGICAL);
}
static SEXP int_slice(SEXP x, SEXP index) {
  SLICE(INTSXP, int, INTEGER, INTEGER_RO, NA_INTEGER);
}
static SEXP dbl_slice(SEXP x, SEXP index) {
  SLICE(REALSXP, double, REAL, REAL_RO, NA_REAL);
}
static SEXP cpl_slice(SEXP x, SEXP index) {
  SLICE(CPLXSXP, Rcomplex, COMPLEX, COMPLEX_RO, vctrs_shared_na_cpl);
}
static SEXP chr_slice(SEXP x, SEXP index) {
  SLICE(STRSXP, SEXP, STRING_PTR, STRING_PTR_RO, NA_STRING);
}
static SEXP raw_slice(SEXP x, SEXP index) {
  SLICE(RAWSXP, Rbyte, RAW, RAW_RO, 0);
}

#undef SLICE
#undef SLICE_COMPACT_REP
#undef SLICE_COMPACT_SEQ
#undef SLICE_INDEX

#define SLICE_BARRIER_INDEX(RTYPE, GET, SET, NA_VALUE)          \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                 \
                                                                \
  for (R_len_t i = 0; i < n; ++i, ++index_data) {               \
    int j = *index_data;                                        \
    SEXP elt = (j == NA_INTEGER) ? NA_VALUE : GET(x, j - 1);    \
    SET(out, i, elt);                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out


#define SLICE_BARRIER_COMPACT_REP(RTYPE, GET, SET, NA_VALUE)    \
  int* index_data = INTEGER(index);                             \
  R_len_t j = index_data[0];                                    \
  R_len_t n = index_data[1];                                    \
                                                                \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                 \
                                                                \
  SEXP elt = (j == NA_INTEGER) ? NA_VALUE : GET(x, j - 1);      \
                                                                \
  for (R_len_t i = 0; i < n; ++i) {                             \
    SET(out, i, elt);                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define SLICE_BARRIER_COMPACT_SEQ(RTYPE, GET, SET)  \
  int* index_data = INTEGER(index);                 \
  R_len_t start = index_data[0];                    \
  R_len_t n = index_data[1];                        \
  R_len_t step = index_data[2];                     \
                                                    \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));     \
                                                    \
  for (R_len_t i = 0; i < n; ++i, start += step) {  \
    SET(out, i, GET(x, start));                     \
  }                                                 \
                                                    \
  UNPROTECT(1);                                     \
  return out

#define SLICE_BARRIER(RTYPE, GET, SET, NA_VALUE)            \
  if (is_compact_rep(index)) {                              \
    SLICE_BARRIER_COMPACT_REP(RTYPE, GET, SET, NA_VALUE);   \
  } else if (is_compact_seq(index)) {                       \
    SLICE_BARRIER_COMPACT_SEQ(RTYPE, GET, SET);             \
  } else {                                                  \
    SLICE_BARRIER_INDEX(RTYPE, GET, SET, NA_VALUE);         \
  }

static SEXP list_slice(SEXP x, SEXP index) {
  SLICE_BARRIER(VECSXP, VECTOR_ELT, SET_VECTOR_ELT, R_NilValue);
}

#undef SLICE_BARRIER
#undef SLICE_BARRIER_COMPACT_REP
#undef SLICE_BARRIER_COMPACT_SEQ
#undef SLICE_BARRIER_INDEX

static SEXP df_slice_init(int n, SEXP names, SEXP row_names) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  // FIXME: Should that be restored?
  Rf_setAttrib(out, R_NamesSymbol, names);

  if (TYPEOF(row_names) == STRSXP) {
    Rf_setAttrib(out, R_RowNamesSymbol, row_names);
  }

  UNPROTECT(1);
  return out;
}

static SEXP df_slice(SEXP x, SEXP index) {
  int nprot = 0;

  R_len_t n = Rf_length(x);

  SEXP names = PROTECT_N(Rf_getAttrib(x, R_NamesSymbol), &nprot);

  SEXP row_names = PROTECT_N(Rf_getAttrib(x, R_RowNamesSymbol), &nprot);
  if (TYPEOF(row_names) == STRSXP) {
    row_names = PROTECT_N(slice_rownames(row_names, index), &nprot);
  }

  SEXP out = PROTECT_N(df_slice_init(n, names, row_names), &nprot);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    SEXP sliced = vec_slice_impl(elt, index);
    SET_VECTOR_ELT(out, i, sliced);
  }

  UNPROTECT(nprot);
  return out;
}


static SEXP vec_slice_fallback(SEXP x, SEXP index) {
  return vctrs_dispatch2(syms_vec_slice_fallback, fns_vec_slice_fallback,
                         syms_x, x,
                         syms_i, index);
}

static SEXP vec_slice_base(enum vctrs_type type, SEXP x, SEXP index) {
  switch (type) {
  case vctrs_type_logical:   return lgl_slice(x, index);
  case vctrs_type_integer:   return int_slice(x, index);
  case vctrs_type_double:    return dbl_slice(x, index);
  case vctrs_type_complex:   return cpl_slice(x, index);
  case vctrs_type_character: return chr_slice(x, index);
  case vctrs_type_raw:       return raw_slice(x, index);
  case vctrs_type_list:      return list_slice(x, index);
  default: Rf_error("Internal error: Non-vector base type `%s` in `vec_slice_base()`",
                    vec_type_as_str(type));
  }
}

static SEXP slice_names(SEXP names, SEXP index) {
  if (names == R_NilValue) {
    return names;
  }

  names = PROTECT(chr_slice(names, index));

  // Replace any `NA` name caused by `NA` index with the empty
  // string. It's ok mutate the names vector since it is freshly
  // created (and the empty string is persistently protected anyway).
  R_len_t n = Rf_length(names);
  SEXP* namesp = STRING_PTR(names);
  const int* ip = INTEGER_RO(index);

  for (R_len_t i = 0; i < n; ++i) {
    if (ip[i] == NA_INTEGER) {
      namesp[i] = strings_empty;
    }
  }

  UNPROTECT(1);
  return names;
}
static SEXP slice_rownames(SEXP names, SEXP index) {
  if (names == R_NilValue) {
    return names;
  }

  names = PROTECT(chr_slice(names, index));

  // Rownames can't contain `NA` or duplicates
  names = vec_as_unique_names(names, true);

  UNPROTECT(1);
  return names;
}

SEXP vec_slice_impl(SEXP x, SEXP index) {
  int nprot = 0;

  SEXP restore_size = PROTECT_N(r_int(vec_index_size(index)), &nprot);

  struct vctrs_proxy_info info = vec_proxy_info(x);
  PROTECT_PROXY_INFO(&info, &nprot);

  SEXP data = info.proxy;

  // Fallback to `[` if the class doesn't implement a proxy. This is
  // to be maximally compatible with existing classes.
  if (OBJECT(x) && info.proxy_method == R_NilValue) {
    if (info.type == vctrs_type_scalar) {
      Rf_errorcall(R_NilValue, "Can't slice a scalar");
    }

    if (is_compact(index)) {
      index = PROTECT_N(compact_materialize(index), &nprot);
    }

    SEXP out;

    if (has_dim(x)) {
      out = PROTECT_N(vec_slice_fallback(x, index), &nprot);
    } else {
      out = PROTECT_N(
        vctrs_dispatch2(syms_bracket, fns_bracket, syms_x, x, syms_i, index),
        &nprot
      );
    }

    // Take over attribute restoration only if the `[` method did not
    // restore itself
    if (ATTRIB(out) == R_NilValue) {
      out = vec_restore(out, x, restore_size);
    }

    UNPROTECT(nprot);
    return out;
  }

  switch (info.type) {
  case vctrs_type_null:
    Rf_error("Internal error: Unexpected `NULL` in `vec_slice_impl()`.");

  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list: {
    SEXP out;

    if (has_dim(x)) {
      out = PROTECT_N(vec_slice_shaped(info.type, data, index), &nprot);

      SEXP names = PROTECT_N(Rf_getAttrib(x, R_DimNamesSymbol), &nprot);
      if (names != R_NilValue) {
        names = PROTECT_N(Rf_shallow_duplicate(names), &nprot);
        SEXP row_names = VECTOR_ELT(names, 0);
        row_names = PROTECT_N(slice_names(row_names, index), &nprot);
        SET_VECTOR_ELT(names, 0, row_names);
        Rf_setAttrib(out, R_DimNamesSymbol, names);
      }
    } else {
      out = PROTECT_N(vec_slice_base(info.type, data, index), &nprot);

      SEXP names = PROTECT_N(Rf_getAttrib(x, R_NamesSymbol), &nprot);
      names = PROTECT_N(slice_names(names, index), &nprot);
      Rf_setAttrib(out, R_NamesSymbol, names);
    }

    out = vec_restore(out, x, restore_size);

    UNPROTECT(nprot);
    return out;
  }

  case vctrs_type_dataframe: {
    SEXP out = PROTECT_N(df_slice(data, index), &nprot);
    out = vec_restore(out, x, restore_size);
    UNPROTECT(nprot);
    return out;
  }

  default:
    Rf_error("Internal error: Unexpected type `%s` for vector proxy in `vec_slice()`",
             vec_type_as_str(info.type));
  }
}

// [[export]]
SEXP vctrs_slice(SEXP x, SEXP index) {
  vec_assert(x, args_empty);

  index = PROTECT(vec_as_index(index, vec_size(x), PROTECT(vec_names(x))));
  SEXP out = vec_slice_impl(x, index);

  UNPROTECT(2);
  return out;
}

SEXP vec_slice(SEXP x, SEXP index) {
  return vctrs_slice(x, index);
}

// [[ include("vctrs.h") ]]
SEXP vec_init(SEXP x, R_len_t n) {
  SEXP i = PROTECT(compact_rep(NA_INTEGER, n));

  SEXP out = vec_slice_impl(x, i);

  UNPROTECT(1);
  return out;
}

// Exported for testing
// [[ register() ]]
SEXP vec_slice_seq(SEXP x, SEXP start, SEXP size, SEXP increasing) {
  R_len_t start_ = r_int_get(start, 0);
  R_len_t size_ = r_int_get(size, 0);
  bool increasing_ = r_lgl_get(increasing, 0);

  SEXP index = PROTECT(compact_seq(start_, size_, increasing_));
  SEXP out = vec_slice_impl(x, index);

  UNPROTECT(1);
  return out;
}

static SEXP int_invert_index(SEXP index, R_len_t n);
static SEXP int_filter_zero(SEXP index, R_len_t n_zero);

static SEXP int_as_index(SEXP index, R_len_t n) {
  const int* data = INTEGER_RO(index);
  R_len_t index_n = Rf_length(index);

  // Zeros need to be filtered out from the index vector.
  // `int_invert_index()` filters them out for negative indices, but
  // positive indices need to go through and `int_filter_zero()`.
  R_len_t n_zero = 0;

  for (R_len_t i = 0; i < index_n; ++i, ++data) {
    int elt = *data;
    if (elt < 0 && elt != NA_INTEGER) {
      return int_invert_index(index, n);
    }
    if (elt == 0) {
      ++n_zero;
    }
    if (elt > n) {
      stop_bad_index_length(n, elt);
    }
  }

  if (n_zero) {
    return int_filter_zero(index, n_zero);
  } else {
    return index;
  }
}


static SEXP lgl_as_index(SEXP i, R_len_t n);

static SEXP int_invert_index(SEXP index, R_len_t n) {
  const int* data = INTEGER_RO(index);
  R_len_t index_n = Rf_length(index);

  SEXP sel = PROTECT(Rf_allocVector(LGLSXP, n));
  r_lgl_fill(sel, 1, n);

  int* sel_data = LOGICAL(sel);

  for (R_len_t i = 0; i < index_n; ++i, ++data) {
    int j = *data;

    if (j == NA_INTEGER) {
      Rf_errorcall(R_NilValue, "Can't subset with a mix of negative indices and missing values");
    }
    if (j >= 0) {
      if (j == 0) {
        continue;
      } else {
        Rf_errorcall(R_NilValue, "Can't subset with a mix of negative and positive indices");
      }
    }

    j = -j;
    if (j > n) {
      stop_bad_index_length(n, j);
    }

    sel_data[j - 1] = 0;
  }

  SEXP out = lgl_as_index(sel, n);

  UNPROTECT(1);
  return out;
}

static SEXP int_filter_zero(SEXP index, R_len_t n_zero) {
  R_len_t index_n = vec_size(index);
  const int* data = INTEGER_RO(index);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, index_n - n_zero));
  int* out_data = INTEGER(out);

  for (R_len_t i = 0; i < index_n; ++i, ++data) {
    int elt = *data;
    if (elt != 0) {
      *out_data = elt;
      ++out_data;
    }
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_as_index(SEXP i, R_len_t n) {
  i = PROTECT(vec_cast(i, vctrs_shared_empty_int, args_empty, args_empty));
  i = int_as_index(i, n);

  UNPROTECT(1);
  return i;
}

static SEXP lgl_as_index(SEXP i, R_len_t n) {
  R_len_t index_n = Rf_length(i);

  if (index_n == n) {
    return r_lgl_which(i, true);
  }

  /* A single `TRUE` or `FALSE` index is recycled to the full vector
   * size. This means `TRUE` is synonym for missing index (i.e. no
   * subsetting) and `FALSE` is synonym for empty index.
   *
   * We could return the missing argument as sentinel to avoid
   * materialising the index vector for the `TRUE` case but this would
   * make `vec_as_index()` an option type just to optimise a rather
   * uncommon case.
   */
  if (index_n == 1) {
    int elt = LOGICAL(i)[0];
    if (elt == NA_LOGICAL) {
      SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
      r_int_fill(out, NA_INTEGER, n);
      UNPROTECT(1);
      return out;
    } else if (elt) {
      SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
      r_int_fill_seq(out, 1, n);
      UNPROTECT(1);
      return out;
    } else {
      return vctrs_shared_empty_int;
    }
  }

  Rf_errorcall(R_NilValue,
               "Logical indices must have length 1 or be as long as the indexed vector.\n"
               "The vector has size %d whereas the index has size %d.",
               n, index_n);
}

static SEXP chr_as_index(SEXP i, SEXP names) {
  if (names == R_NilValue) {
    Rf_errorcall(R_NilValue, "Can't use character to index an unnamed vector.");
  }

  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector.");
  }

  SEXP matched = PROTECT(Rf_match(names, i, NA_INTEGER));

  R_len_t n = Rf_length(matched);
  const int* p = INTEGER_RO(matched);
  const SEXP* ip = STRING_PTR_RO(i);

  for (R_len_t k = 0; k < n; ++k) {
    if (p[k] == NA_INTEGER && ip[k] != NA_STRING) {
      Rf_errorcall(R_NilValue, "Can't index non-existing elements.");
    }
  }

  UNPROTECT(1);
  return matched;
}

SEXP vec_as_index(SEXP i, R_len_t n, SEXP names) {
  switch (TYPEOF(i)) {
  case NILSXP: return vctrs_shared_empty_int;
  case INTSXP: return int_as_index(i, n);
  case REALSXP: return dbl_as_index(i, n);
  case LGLSXP: return lgl_as_index(i, n);
  case STRSXP: return chr_as_index(i, names);

  default: Rf_errorcall(R_NilValue, "`i` must be an integer, character, or logical vector, not a %s.",
                        Rf_type2char(TYPEOF(i)));
  }
}

SEXP vctrs_as_index(SEXP i, SEXP n, SEXP names) {
  return vec_as_index(i, r_int_get(n, 0), names);
}

SEXP split_along_fallback_shaped(SEXP x, SEXP indices, bool check_index) {
  bool has_indices = indices != R_NilValue;

  R_len_t x_size = vec_size(x);
  R_len_t out_size = has_indices ? vec_size(indices) : x_size;

  PROTECT_INDEX index_prot_idx;
  SEXP index = r_int(0);
  PROTECT_WITH_INDEX(index, &index_prot_idx);
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, out_size));

  SEXP row_names = PROTECT(vec_names(x));

  for (R_len_t i = 0; i < out_size; ++i) {
    if (has_indices) {
      index = VECTOR_ELT(indices, i);

      if (check_index) {
        index = vec_as_index(index, x_size, row_names);
        REPROTECT(index, index_prot_idx);
      }
    } else {
      ++(*p_index);
    }

    // `vec_slice_fallback()` will also `vec_restore()` for us
    elt = vec_slice_fallback(x, index);
    REPROTECT(elt, elt_prot_idx);

    SET_VECTOR_ELT(out, i, elt);
  }

  UNPROTECT(4);
  return out;
}

SEXP split_along_fallback(SEXP x, SEXP indices, bool check_index) {
  bool has_indices = indices != R_NilValue;

  R_len_t x_size = vec_size(x);
  R_len_t out_size = has_indices ? vec_size(indices) : x_size;

  SEXP restore_size = PROTECT(r_int(1));
  int* p_restore_size = INTEGER(restore_size);

  PROTECT_INDEX index_prot_idx;
  SEXP index = r_int(0);
  PROTECT_WITH_INDEX(index, &index_prot_idx);
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, out_size));

  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  // Construct call with symbols, not values, for performance
  SEXP call = PROTECT(Rf_lang3(syms_bracket, syms_x, syms_i));

  SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 2));
  Rf_defineVar(syms_bracket, fns_bracket, env);
  Rf_defineVar(syms_x, x, env);
  Rf_defineVar(syms_i, index, env);

  for (R_len_t i = 0; i < out_size; ++i) {
    if (has_indices) {
      index = VECTOR_ELT(indices, i);
      *p_restore_size = vec_size(index);

      if (check_index) {
        index = vec_as_index(index, x_size, names);
        REPROTECT(index, index_prot_idx);
      }

      // Must redefine after altering
      Rf_defineVar(syms_i, index, env);
    } else {
      ++(*p_index);
    }

    elt = Rf_eval(call, env);
    REPROTECT(elt, elt_prot_idx);

    // Restore attributes only if `[` fallback doesn't
    if (ATTRIB(elt) == R_NilValue) {
      elt = vec_restore(elt, x, restore_size);
    }

    SET_VECTOR_ELT(out, i, elt);
  }

  UNPROTECT(7);
  return out;
}

SEXP split_along_shaped(SEXP x, SEXP indices, bool check_index, struct vctrs_proxy_info info) {
  bool has_indices = indices != R_NilValue;

  R_len_t x_size = vec_size(x);
  R_len_t out_size = has_indices ? vec_size(indices) : x_size;

  SEXP restore_size = PROTECT(r_int(1));
  int* p_restore_size = INTEGER(restore_size);

  PROTECT_INDEX index_prot_idx;
  SEXP index = r_int(0);
  PROTECT_WITH_INDEX(index, &index_prot_idx);
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, out_size));

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

  for (R_len_t i = 0; i < out_size; ++i) {
    if (has_indices) {
      index = VECTOR_ELT(indices, i);
      *p_restore_size = vec_size(index);

      if (check_index) {
        index = vec_as_index(index, x_size, row_names);
        REPROTECT(index, index_prot_idx);
      }
    } else {
      ++(*p_index);
    }

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

SEXP split_along(SEXP x, SEXP indices, bool check_index, struct vctrs_proxy_info info) {
  bool has_indices = indices != R_NilValue;

  R_len_t x_size = vec_size(x);
  R_len_t out_size = has_indices ? vec_size(indices) : x_size;

  SEXP restore_size = PROTECT(r_int(1));
  int* p_restore_size = INTEGER(restore_size);

  PROTECT_INDEX index_prot_idx;
  SEXP index = r_int(0);
  PROTECT_WITH_INDEX(index, &index_prot_idx);
  int* p_index = INTEGER(index);

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, out_size));

  SEXP data = info.proxy;

  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  PROTECT_INDEX name_prot_idx;
  SEXP name = R_NilValue;
  PROTECT_WITH_INDEX(name, &name_prot_idx);

  for (R_len_t i = 0; i < out_size; ++i) {
    if (has_indices) {
      index = VECTOR_ELT(indices, i);
      *p_restore_size = vec_size(index);

      if (check_index) {
        index = vec_as_index(index, x_size, names);
        REPROTECT(index, index_prot_idx);
      }
    } else {
      ++(*p_index);
    }

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

SEXP split_along_df(SEXP x, SEXP indices, bool check_index, struct vctrs_proxy_info info) {
  bool has_indices = indices != R_NilValue;

  R_len_t x_size = vec_size(x);
  R_len_t out_size = has_indices ? vec_size(indices) : x_size;

  PROTECT_INDEX index_prot_idx;
  SEXP index = r_int(0);
  PROTECT_WITH_INDEX(index, &index_prot_idx);
  int* p_index = INTEGER(index);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, out_size));

  SEXP data = info.proxy;

  int n_col = Rf_length(x);
  SEXP names = Rf_getAttrib(x, R_NamesSymbol);
  SEXP row_names = Rf_getAttrib(x, R_RowNamesSymbol);

  PROTECT_INDEX sliced_rownames_prot_idx;
  SEXP sliced_rownames = R_NilValue;
  PROTECT_WITH_INDEX(sliced_rownames, &sliced_rownames_prot_idx);

  bool has_row_names = TYPEOF(row_names) == STRSXP;

  // Pre-load the `out` container with lists that will become data frames
  for (int i = 0; i < out_size; ++i) {
    if (has_indices) {
      index = VECTOR_ELT(indices, i);

      if (check_index) {
        index = vec_as_index(index, x_size, row_names);
        REPROTECT(index, index_prot_idx);
      }
    } else {
      ++(*p_index);
    }

    if (has_row_names) {
      sliced_rownames = slice_rownames(row_names, index);
      REPROTECT(sliced_rownames, sliced_rownames_prot_idx);
    }

    SEXP elt = df_slice_init(n_col, names, sliced_rownames);
    SET_VECTOR_ELT(out, i, elt);
  }

  // Split each column according to the indices, and then assign the results
  // into the appropriate data frame column in the `out` list
  for (int i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(data, i);
    SEXP split_col = PROTECT(vec_split_along(col, indices, false));

    for (int j = 0; j < out_size; ++j) {
      SEXP out_elt = VECTOR_ELT(out, j);
      SET_VECTOR_ELT(out_elt, i, VECTOR_ELT(split_col, j));
    }

    UNPROTECT(1);
  }

  // Restore each data frame
  for (int i = 0; i < out_size; ++i) {
    SEXP out_elt = VECTOR_ELT(out, i);
    out_elt = vec_restore(out_elt, x, R_NilValue);
    SET_VECTOR_ELT(out, i, out_elt);
  }

  UNPROTECT(3);
  return out;
}

// Used in `as_df_row()` to turn `x` into a list like:
// list(vec_slice(x, 1L), vec_slice(x, 2L), ...)
// but in a more efficient way
// [[ include("vctrs.h"); register() ]]
SEXP vec_split_along(SEXP x, SEXP indices, bool check_index) {
  if (indices != R_NilValue && TYPEOF(indices) != VECSXP) {
    Rf_errorcall(R_NilValue, "`indices` must be a list of integers.");
  }

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
      return split_along_fallback_shaped(x, indices, check_index);
    }

    UNPROTECT(nprot);
    return split_along_fallback(x, indices, check_index);
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
      return split_along_shaped(x, indices, check_index, info);
    }

    UNPROTECT(nprot);
    return split_along(x, indices, check_index, info);
  }

  case vctrs_type_dataframe: {
    UNPROTECT(nprot);
    return split_along_df(x, indices, check_index, info);
  }
  default:
    vec_assert(x, args_empty);
    Rf_error("Internal error: Unexpected type `%s` for vector proxy in `vec_split_along()`",
             vec_type_as_str(info.type));
  }
}

SEXP vctrs_split_along(SEXP x, SEXP indices) {
  return vec_split_along(x, indices, true);
}

void vctrs_init_slice(SEXP ns) {
  syms_vec_slice_fallback = Rf_install("vec_slice_fallback");
  fns_vec_slice_fallback = Rf_findVar(syms_vec_slice_fallback, ns);
}
