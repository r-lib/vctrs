#include "vctrs.h"
#include "utils.h"
#include "slice.h"

// Initialised at load time
SEXP syms_vec_slice_fallback = NULL;
SEXP fns_vec_slice_fallback = NULL;

// Defined below
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

static SEXP df_slice(SEXP x, SEXP index) {
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

static SEXP int_as_index(SEXP index, R_len_t n,
                         struct vec_as_index_options* opts) {
  const int* data = INTEGER_RO(index);
  R_len_t index_n = Rf_length(index);

  // Zeros need to be filtered out from the index vector.
  // `int_invert_index()` filters them out for negative indices, but
  // positive indices need to go through and `int_filter_zero()`.
  R_len_t n_zero = 0;

  bool convert_negative = opts->convert_negative;

  for (R_len_t i = 0; i < index_n; ++i, ++data) {
    int elt = *data;
    if (convert_negative && elt < 0 && elt != NA_INTEGER) {
      return int_invert_index(index, n);
    }
    if (elt == 0) {
      ++n_zero;
    }
    if (abs(elt) > n) {
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

static SEXP dbl_as_index(SEXP i, R_len_t n, struct vec_as_index_options* opts) {
  i = PROTECT(vec_cast(i, vctrs_shared_empty_int, args_empty, args_empty));
  i = int_as_index(i, n, opts);

  UNPROTECT(1);
  return i;
}

static SEXP lgl_as_index(SEXP i, R_len_t n) {
  R_len_t index_n = Rf_length(i);

  if (index_n == n) {
    SEXP out = PROTECT(r_lgl_which(i, true));

    SEXP nms = PROTECT(r_names(i));
    if (nms != R_NilValue) {
      nms = PROTECT(vec_slice(nms, out));
      r_poke_names(out, nms);
      UNPROTECT(1);
    }

    UNPROTECT(2);
    return out;
  }

  /* A single `TRUE` or `FALSE` index is recycled_nms to the full vector
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

    SEXP out;
    if (elt == NA_LOGICAL) {
      out = PROTECT(Rf_allocVector(INTSXP, n));
      r_int_fill(out, NA_INTEGER, n);
    } else if (elt) {
      out = PROTECT(Rf_allocVector(INTSXP, n));
      r_int_fill_seq(out, 1, n);
    } else {
      return vctrs_shared_empty_int;
    }

    SEXP nms = PROTECT(r_names(i));
    if (nms != R_NilValue) {
      SEXP recycled_nms = PROTECT(Rf_allocVector(STRSXP, n));
      r_chr_fill(recycled_nms, r_chr_get(nms, 0), n);
      r_poke_names(out, recycled_nms);
      UNPROTECT(1);
    }

    UNPROTECT(2);
    return out;
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

  r_poke_names(matched, PROTECT(r_names(i))); UNPROTECT(1);

  UNPROTECT(1);
  return matched;
}

SEXP vec_as_index(SEXP i, R_len_t n, SEXP names) {
  struct vec_as_index_options opts = { .convert_negative = true };
  return vec_as_index_opts(i, n, names, &opts);
}

SEXP vec_as_index_opts(SEXP i, R_len_t n, SEXP names,
                       struct vec_as_index_options* opts) {
  switch (TYPEOF(i)) {
  case NILSXP: return vctrs_shared_empty_int;
  case INTSXP: return int_as_index(i, n, opts);
  case REALSXP: return dbl_as_index(i, n, opts);
  case LGLSXP: return lgl_as_index(i, n);
  case STRSXP: return chr_as_index(i, names);

  default: Rf_errorcall(R_NilValue, "`i` must be an integer, character, or logical vector, not a %s.",
                        Rf_type2char(TYPEOF(i)));
  }
}

SEXP vctrs_as_index(SEXP i, SEXP n, SEXP names, SEXP convert_negative) {
  if (!r_is_bool(convert_negative)) {
    Rf_error("Internal error: `convert_negative` must be a boolean");
  }
  struct vec_as_index_options opts = { .convert_negative = LOGICAL(convert_negative)[0]};
  return vec_as_index_opts(i, r_int_get(n, 0), names, &opts);
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

void vctrs_init_slice(SEXP ns) {
  syms_vec_slice_fallback = Rf_install("vec_slice_fallback");
  fns_vec_slice_fallback = Rf_findVar(syms_vec_slice_fallback, ns);
}
