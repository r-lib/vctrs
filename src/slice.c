#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_slice_dispatch = NULL;
SEXP fns_vec_slice_dispatch = NULL;

// Defined below
SEXP vec_as_index(SEXP i, SEXP x);
static void slice_names(SEXP x, SEXP to, SEXP index);
static SEXP vec_slice_impl(SEXP x, SEXP index, bool dispatch);


static void stop_bad_index_length(R_len_t data_n, R_len_t i) {
  Rf_errorcall(R_NilValue,
               "Can't index beyond the end of a vector.\n"
               "The vector has length %d and you've tried to subset element %d.",
               data_n, i);
}

#define SLICE(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)       \
  const CTYPE* data = CONST_DEREF(x);                           \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                 \
  CTYPE* out_data = DEREF(out);                                 \
                                                                \
  for (R_len_t i = 0; i < n; ++i, ++index_data, ++out_data) {   \
    int j = *index_data;                                        \
    *out_data = (j == NA_INTEGER) ? NA_VALUE : data[j - 1];     \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

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


#define SLICE_BARRIER(RTYPE, GET, SET, NA_VALUE)                \
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

static SEXP list_slice(SEXP x, SEXP index) {
  SLICE_BARRIER(VECSXP, VECTOR_ELT, SET_VECTOR_ELT, R_NilValue);
}

#undef SLICE_BARRIER

static SEXP df_slice(SEXP x, SEXP index) {
  R_len_t n = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  SEXP nms = Rf_getAttrib(x, R_NamesSymbol);
  Rf_setAttrib(out, R_NamesSymbol, nms);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP sliced = vec_slice_impl(VECTOR_ELT(x, i), index, true);
    SET_VECTOR_ELT(out, i, sliced);
  }

  UNPROTECT(1);
  return out;
}


static SEXP vec_slice_dispatch(SEXP x, SEXP index) {
  SEXP out = PROTECT(vctrs_dispatch2(syms_vec_slice_dispatch, fns_vec_slice_dispatch,
                                     syms_x, x,
                                     syms_i, index));
  out = vctrs_restore(out, x, index);

  UNPROTECT(1);
  return out;
}

static SEXP vec_slice_impl(SEXP x, SEXP index, bool dispatch) {
  if (has_dim(x)) {
    return vec_slice_dispatch(x, index);
  }

  SEXP out = R_NilValue;

  switch (vec_typeof_impl(x, dispatch)) {
  case vctrs_type_null:
    Rf_error("Internal error: Unexpected `NULL` in `vec_slice_impl()`.");

  case vctrs_type_logical:
    out = lgl_slice(x, index);
    break;
  case vctrs_type_integer:
    out = int_slice(x, index);
    break;
  case vctrs_type_double:
    out = dbl_slice(x, index);
    break;
  case vctrs_type_complex:
    out = cpl_slice(x, index);
    break;
  case vctrs_type_character:
    out = chr_slice(x, index);
    break;
  case vctrs_type_raw:
    out = raw_slice(x, index);
    break;
  case vctrs_type_list:
    out = list_slice(x, index);
    break;

  case vctrs_type_dataframe:
    out = PROTECT(df_slice(x, index));
    out = vctrs_restore(out, x, index);
    UNPROTECT(1);
    return out;

  default:
    return vec_slice_dispatch(x, index);
  }

  PROTECT(out);
  slice_names(out, x, index);

  out = vctrs_restore(out, x, index);

  UNPROTECT(1);
  return out;
}

static void slice_names(SEXP x, SEXP to, SEXP index) {
  SEXP nms = PROTECT(Rf_getAttrib(to, R_NamesSymbol));

  if (nms == R_NilValue) {
    UNPROTECT(1);
    return;
  }

  nms = PROTECT(chr_slice(nms, index));
  Rf_setAttrib(x, R_NamesSymbol, nms);

  UNPROTECT(2);
}

SEXP vctrs_slice(SEXP x, SEXP index, SEXP dispatch) {
  if (x == R_NilValue || index == R_MissingArg) {
    return x;
  }

  index = PROTECT(vec_as_index(index, x));
  SEXP out = vec_slice_impl(x, index, *LOGICAL(dispatch));

  UNPROTECT(1);
  return out;
}
SEXP vec_slice(SEXP x, SEXP index) {
  return vctrs_slice(x, index, vctrs_shared_true);
}

static SEXP int_invert_index(SEXP index, SEXP x);
static SEXP int_filter_zero(SEXP index, R_len_t x);

static SEXP int_as_index(SEXP index, SEXP x) {
  const int* data = INTEGER_RO(index);
  R_len_t n = Rf_length(index);
  R_len_t vec_n = vec_size(x);

  // Zeros need to be filtered out from the index vector.
  // `int_invert_index()` filters them out for negative indices, but
  // positive indices need to go through and `int_filter_zero()`.
  R_len_t n_zero = 0;

  for (R_len_t i = 0; i < n; ++i, ++data) {
    int elt = *data;
    if (elt < 0 && elt != NA_INTEGER) {
      return int_invert_index(index, x);
    }
    if (elt == 0) {
      ++n_zero;
    }
    if (elt > vec_n) {
      stop_bad_index_length(vec_n, elt);
    }
  }

  if (n_zero) {
    return int_filter_zero(index, n_zero);
  } else {
    return index;
  }
}


static SEXP lgl_as_index(SEXP i, SEXP x);

static SEXP int_invert_index(SEXP index, SEXP x) {
  const int* data = INTEGER_RO(index);
  R_len_t n = Rf_length(index);
  R_len_t vec_n = vec_size(x);

  SEXP sel = PROTECT(Rf_allocVector(LGLSXP, vec_size(x)));
  r_lgl_fill(sel, 1);

  int* sel_data = LOGICAL(sel);

  for (R_len_t i = 0; i < n; ++i, ++data) {
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
    if (j > vec_n) {
      stop_bad_index_length(vec_n, j);
    }

    sel_data[j - 1] = 0;
  }

  SEXP out = lgl_as_index(sel, x);

  UNPROTECT(1);
  return out;
}

static SEXP int_filter_zero(SEXP index, R_len_t n_zero) {
  R_len_t n = vec_size(index);
  const int* data = INTEGER_RO(index);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, n - n_zero));
  int* out_data = INTEGER(out);

  for (R_len_t i = 0; i < n; ++i, ++data) {
    int elt = *data;
    if (elt != 0) {
      *out_data = elt;
      ++out_data;
    }
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_as_index(SEXP i, SEXP x) {
  i = PROTECT(vec_cast(i, vctrs_shared_empty_int));
  i = int_as_index(i, x);

  UNPROTECT(1);
  return i;
}

static SEXP lgl_as_index(SEXP i, SEXP x) {
  R_len_t n = Rf_length(i);
  R_len_t n_vec = vec_size(x);

  if (n == n_vec) {
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
  if (n == 1) {
    int elt = *LOGICAL(i);
    if (elt == NA_LOGICAL) {
      SEXP out = PROTECT(Rf_allocVector(INTSXP, n_vec));
      r_int_fill(out, NA_INTEGER);
      UNPROTECT(1);
      return out;
    } else if (elt) {
      SEXP out = PROTECT(Rf_allocVector(INTSXP, n_vec));
      r_int_fill_seq(out, 1);
      UNPROTECT(1);
      return out;
    } else {
      return vctrs_shared_empty_int;
    }
  }

  Rf_errorcall(R_NilValue,
               "Logical indices must have length 1 or be as long as the indexed vector.\n"
               "The vector has size %d whereas the index has size %d.",
               n, vec_size(x));
}

static SEXP chr_as_index(SEXP i, SEXP x) {
  SEXP nms = Rf_getAttrib(x, R_NamesSymbol);

  if (nms == R_NilValue) {
    Rf_errorcall(R_NilValue, "Can't use character to index an unnamed vector.");
  }

  i = PROTECT(Rf_match(nms, i, NA_INTEGER));

  if (r_int_any_na(i)) {
    Rf_errorcall(R_NilValue, "Can't index non-existing elements.");
  }

  UNPROTECT(1);
  return i;
}

SEXP vec_as_index(SEXP i, SEXP x) {
  switch (TYPEOF(i)) {
  case INTSXP: return int_as_index(i, x);
  case REALSXP: return dbl_as_index(i, x);
  case LGLSXP: return lgl_as_index(i, x);
  case STRSXP: return chr_as_index(i, x);
  case SYMSXP: if (i == R_MissingArg) return i;

  default: Rf_errorcall(R_NilValue, "`i` must be an integer, character, or logical vector, not a %s.",
                        Rf_type2char(TYPEOF(i)));
  }
}


void vctrs_init_size(SEXP ns) {
  syms_vec_slice_dispatch = Rf_install("vec_slice_dispatch");
  fns_vec_slice_dispatch = Rf_findVar(syms_vec_slice_dispatch, ns);
}
