#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_slice_fallback = NULL;
SEXP fns_vec_slice_fallback = NULL;

// Defined below
SEXP vec_as_index(SEXP i, R_len_t n, SEXP names);

/**
 * This `vec_slice()` variant falls back to `[` with S3 objects.
 *
 * @param to The type to restore to.
 * @param dispatch When `true`, dispatches to `[` for compatibility
 *   with base R. When `false`, uses native implementations.
 */
SEXP vec_slice_impl(SEXP x, SEXP index);


static void stop_index_oob_positions(SEXP i, R_len_t size) {
  SEXP size_obj = PROTECT(r_int(size));
  vctrs_eval_mask2(Rf_install("stop_index_oob_positions"),
                   syms_i, i,
                   syms_size, size_obj,
                   vctrs_ns_env);

  UNPROTECT(1);
  never_reached("stop_index_oob_positions");
}
static void stop_index_oob_names(SEXP i, SEXP names) {
  vctrs_eval_mask2(Rf_install("stop_index_oob_names"),
                   syms_i, i,
                   syms_names, names,
                   vctrs_ns_env);

  UNPROTECT(1);
  never_reached("stop_index_oob_names");
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

SEXP df_slice(SEXP x, SEXP index) {
  R_len_t n = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  // FIXME: Should that be restored?
  SEXP nms = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, nms);
  UNPROTECT(1);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    SEXP sliced = vec_slice_impl(elt, index);
    SET_VECTOR_ELT(out, i, sliced);
  }

  SEXP row_nms = PROTECT(get_rownames(x));
  if (TYPEOF(row_nms) == STRSXP) {
    row_nms = PROTECT(slice_rownames(row_nms, index));
    Rf_setAttrib(out, R_RowNamesSymbol, row_nms);
    UNPROTECT(1);
  }
  UNPROTECT(1);

  UNPROTECT(1);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_slice_fallback(SEXP x, SEXP index) {
  return vctrs_dispatch2(syms_vec_slice_fallback, fns_vec_slice_fallback,
                         syms_x, x,
                         syms_i, index);
}

// [[ include("vctrs.h") ]]
SEXP vec_slice_base(enum vctrs_type type, SEXP x, SEXP index) {
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

// [[ include("vctrs.h") ]]
SEXP slice_names(SEXP names, SEXP index) {
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

// [[ include("vctrs.h") ]]
SEXP slice_rownames(SEXP names, SEXP index) {
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
      stop_index_oob_positions(index, n);
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
      stop_index_oob_positions(index, n);
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
      stop_index_oob_names(i, names);
    }
  }

  UNPROTECT(1);
  return matched;
}

// [[ include("vctrs.h") ]]
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

void vctrs_init_slice(SEXP ns) {
  syms_vec_slice_fallback = Rf_install("vec_slice_fallback");
  fns_vec_slice_fallback = Rf_findVar(syms_vec_slice_fallback, ns);
}
