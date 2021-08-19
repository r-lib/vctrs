#include <rlang.h>
#include "vctrs.h"
#include "altrep.h"
#include "slice.h"
#include "subscript-loc.h"
#include "type-data-frame.h"
#include "owned.h"
#include "utils.h"
#include "dim.h"
#include "assert.h"

// Initialised at load time
SEXP syms_vec_slice_fallback = NULL;
SEXP fns_vec_slice_fallback = NULL;

SEXP syms_vec_slice_fallback_integer64 = NULL;
SEXP fns_vec_slice_fallback_integer64 = NULL;
SEXP syms_vec_slice_dispatch_integer64 = NULL;
SEXP fns_vec_slice_dispatch_integer64 = NULL;


#define SLICE_SUBSCRIPT(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)     \
  const CTYPE* data = CONST_DEREF(x);                                   \
  R_len_t n = Rf_length(subscript);                                     \
  int* subscript_data = INTEGER(subscript);                             \
                                                                        \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                         \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  for (R_len_t i = 0; i < n; ++i, ++subscript_data, ++out_data) {       \
    int j = *subscript_data;                                            \
    *out_data = (j == NA_INTEGER) ? NA_VALUE : data[j - 1];             \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out

#define SLICE_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)   \
  const CTYPE* data = CONST_DEREF(x);                                   \
                                                                        \
  int* subscript_data = INTEGER(subscript);                             \
  R_len_t j = subscript_data[0];                                        \
  R_len_t n = subscript_data[1];                                        \
                                                                        \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                         \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  CTYPE elt = (j == NA_INTEGER) ? NA_VALUE : data[j - 1];               \
                                                                        \
  for (R_len_t i = 0; i < n; ++i, ++out_data) {                         \
    *out_data = elt;                                                    \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out

#define SLICE_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF)     \
  int* subscript_data = INTEGER(subscript);                     \
  R_len_t start = subscript_data[0];                            \
  R_len_t n = subscript_data[1];                                \
  R_len_t step = subscript_data[2];                             \
                                                                \
  const CTYPE* data = CONST_DEREF(x) + start;                   \
                                                                \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                 \
  CTYPE* out_data = DEREF(out);                                 \
                                                                \
  for (int i = 0; i < n; ++i, ++out_data, data += step) {       \
    *out_data = *data;                                          \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define SLICE(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)                   \
  if (ALTREP(x)) {                                                          \
    SEXP alt_subscript = PROTECT(compact_materialize(subscript));           \
    SEXP out = ALTVEC_EXTRACT_SUBSET_PROXY(x, alt_subscript, R_NilValue);   \
    UNPROTECT(1);                                                           \
    if (out != NULL) {                                                      \
      return out;                                                           \
    }                                                                       \
  }                                                                         \
  if (is_compact_rep(subscript)) {                                          \
    SLICE_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE);          \
  } else if (is_compact_seq(subscript)) {                                   \
    SLICE_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF);                    \
  } else {                                                                  \
    SLICE_SUBSCRIPT(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE);            \
  }

static SEXP lgl_slice(SEXP x, SEXP subscript) {
  SLICE(LGLSXP, int, LOGICAL, LOGICAL_RO, NA_LOGICAL);
}
static SEXP int_slice(SEXP x, SEXP subscript) {
  SLICE(INTSXP, int, INTEGER, INTEGER_RO, NA_INTEGER);
}
static SEXP dbl_slice(SEXP x, SEXP subscript) {
  SLICE(REALSXP, double, REAL, REAL_RO, NA_REAL);
}
static SEXP cpl_slice(SEXP x, SEXP subscript) {
  SLICE(CPLXSXP, Rcomplex, COMPLEX, COMPLEX_RO, vctrs_shared_na_cpl);
}
static SEXP chr_slice(SEXP x, SEXP subscript) {
  SLICE(STRSXP, SEXP, STRING_PTR, STRING_PTR_RO, NA_STRING);
}
static SEXP raw_slice(SEXP x, SEXP subscript) {
  SLICE(RAWSXP, Rbyte, RAW, RAW_RO, 0);
}

#undef SLICE
#undef SLICE_COMPACT_REP
#undef SLICE_COMPACT_SEQ
#undef SLICE_SUBSCRIPT

#define SLICE_BARRIER_SUBSCRIPT(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE)  \
  const CTYPE* data = CONST_DEREF(x);                                      \
                                                                           \
  R_len_t n = Rf_length(subscript);                                        \
  int* subscript_data = INTEGER(subscript);                                \
                                                                           \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                            \
                                                                           \
  for (R_len_t i = 0; i < n; ++i, ++subscript_data) {                      \
    int j = *subscript_data;                                               \
    SEXP elt = (j == NA_INTEGER) ? NA_VALUE : data[j - 1];                 \
    SET(out, i, elt);                                                      \
  }                                                                        \
                                                                           \
  UNPROTECT(1);                                                            \
  return out


#define SLICE_BARRIER_COMPACT_REP(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE)    \
  const CTYPE* data = CONST_DEREF(x);                                          \
                                                                               \
  int* subscript_data = INTEGER(subscript);                                    \
  R_len_t j = subscript_data[0];                                               \
  R_len_t n = subscript_data[1];                                               \
                                                                               \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                                \
                                                                               \
  SEXP elt = (j == NA_INTEGER) ? NA_VALUE : data[j - 1];                       \
                                                                               \
  for (R_len_t i = 0; i < n; ++i) {                                            \
    SET(out, i, elt);                                                          \
  }                                                                            \
                                                                               \
  UNPROTECT(1);                                                                \
  return out

#define SLICE_BARRIER_COMPACT_SEQ(RTYPE, CTYPE, CONST_DEREF, SET)  \
  const CTYPE* data = CONST_DEREF(x);                              \
                                                                   \
  int* subscript_data = INTEGER(subscript);                        \
  R_len_t start = subscript_data[0];                               \
  R_len_t n = subscript_data[1];                                   \
  R_len_t step = subscript_data[2];                                \
                                                                   \
  SEXP out = PROTECT(Rf_allocVector(RTYPE, n));                    \
                                                                   \
  for (R_len_t i = 0; i < n; ++i, start += step) {                 \
    SET(out, i, data[start]);                                      \
  }                                                                \
                                                                   \
  UNPROTECT(1);                                                    \
  return out

#define SLICE_BARRIER(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE)          \
  if (is_compact_rep(subscript)) {                                       \
    SLICE_BARRIER_COMPACT_REP(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE); \
  } else if (is_compact_seq(subscript)) {                                \
    SLICE_BARRIER_COMPACT_SEQ(RTYPE, CTYPE, CONST_DEREF, SET);           \
  } else {                                                               \
    SLICE_BARRIER_SUBSCRIPT(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE);   \
  }

static SEXP list_slice(SEXP x, SEXP subscript) {
  SLICE_BARRIER(VECSXP, SEXP, VECTOR_PTR_RO, SET_VECTOR_ELT, R_NilValue);
}

#undef SLICE_BARRIER
#undef SLICE_BARRIER_COMPACT_REP
#undef SLICE_BARRIER_COMPACT_SEQ
#undef SLICE_BARRIER_SUBSCRIPT

static SEXP df_slice(SEXP x, SEXP subscript) {
  R_len_t n = Rf_length(x);
  R_len_t size = df_size(x);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  // FIXME: Should that be restored?
  SEXP nms = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, nms);
  UNPROTECT(1);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(x, i);

    if (vec_size(elt) != size) {
      r_stop_internal("df_slice", "Columns must match the data frame size.");
    }

    SEXP sliced = vec_slice_impl(elt, subscript);
    SET_VECTOR_ELT(out, i, sliced);
  }

  SEXP row_nms = PROTECT(df_rownames(x));
  if (TYPEOF(row_nms) == STRSXP) {
    row_nms = PROTECT(slice_rownames(row_nms, subscript));
    Rf_setAttrib(out, R_RowNamesSymbol, row_nms);
    UNPROTECT(1);
  }
  UNPROTECT(1);

  UNPROTECT(1);
  return out;
}


SEXP vec_slice_fallback(SEXP x, SEXP subscript) {
  // TODO - Remove once bit64 is updated on CRAN. Special casing integer64
  // objects to ensure correct slicing with `NA_integer_`.
  if (is_integer64(x)) {
    return vctrs_dispatch2(syms_vec_slice_fallback_integer64, fns_vec_slice_fallback_integer64,
                           syms_x, x,
                           syms_i, subscript);
  }

  return vctrs_dispatch2(syms_vec_slice_fallback, fns_vec_slice_fallback,
                         syms_x, x,
                         syms_i, subscript);
}

static SEXP vec_slice_dispatch(SEXP x, SEXP subscript) {
  // TODO - Remove once bit64 is updated on CRAN. Special casing integer64
  // objects to ensure correct slicing with `NA_integer_`.
  if (is_integer64(x)) {
    return vctrs_dispatch2(syms_vec_slice_dispatch_integer64, fns_vec_slice_dispatch_integer64,
                           syms_x, x,
                           syms_i, subscript);
  }

  return vctrs_dispatch2(syms_bracket, fns_bracket,
                         syms_x, x,
                         syms_i, subscript);
}

bool vec_requires_fallback(SEXP x, struct vctrs_proxy_info info) {
  return OBJECT(x) &&
    info.proxy_method == R_NilValue &&
    info.type != vctrs_type_dataframe;
}

SEXP vec_slice_base(enum vctrs_type type, SEXP x, SEXP subscript) {
  switch (type) {
  case vctrs_type_logical:   return lgl_slice(x, subscript);
  case vctrs_type_integer:   return int_slice(x, subscript);
  case vctrs_type_double:    return dbl_slice(x, subscript);
  case vctrs_type_complex:   return cpl_slice(x, subscript);
  case vctrs_type_character: return chr_slice(x, subscript);
  case vctrs_type_raw:       return raw_slice(x, subscript);
  case vctrs_type_list:      return list_slice(x, subscript);
  default: stop_unimplemented_vctrs_type("vec_slice_base", type);
  }
}

// Replace any `NA` name caused by `NA` subscript with the empty
// string. It's ok mutate the names vector since it is freshly
// created, but we make an additional check for that anyways
// (and the empty string is persistently protected anyway).
static void repair_na_names(SEXP names, SEXP subscript) {
  if (!NO_REFERENCES(names)) {
    r_stop_internal("repair_na_names", "`names` can't be referenced.");
  }

  // No possible way to have `NA_integer_` in a compact seq
  if (is_compact_seq(subscript)) {
    return;
  }

  R_len_t n = Rf_length(names);

  if (n == 0) {
    return;
  }

  const int* p_subscript = INTEGER_RO(subscript);

  // Special handling for a compact_rep object with repeated `NA`
  if (is_compact_rep(subscript)) {
    if (p_subscript[0] != NA_INTEGER) {
      return;
    }

    for (R_len_t i = 0; i < n; ++i) {
      SET_STRING_ELT(names, i, strings_empty);
    }

    return;
  }

  for (R_len_t i = 0; i < n; ++i) {
    if (p_subscript[i] == NA_INTEGER) {
      SET_STRING_ELT(names, i, strings_empty);
    }
  }
}

SEXP slice_names(SEXP names, SEXP subscript) {
  if (names == R_NilValue) {
    return names;
  }

  names = PROTECT(chr_slice(names, subscript));

  repair_na_names(names, subscript);

  UNPROTECT(1);
  return names;
}
SEXP slice_rownames(SEXP names, SEXP subscript) {
  if (names == R_NilValue) {
    return names;
  }

  names = PROTECT(chr_slice(names, subscript));

  // Rownames can't contain `NA` or duplicates
  names = vec_as_unique_names(names, true);

  UNPROTECT(1);
  return names;
}

SEXP vec_slice_impl(SEXP x, SEXP subscript) {
  int nprot = 0;

  SEXP restore_size = PROTECT_N(r_int(vec_subscript_size(subscript)), &nprot);

  struct vctrs_proxy_info info = vec_proxy_info(x);
  PROTECT_PROXY_INFO(&info, &nprot);

  SEXP data = info.proxy;

  // Fallback to `[` if the class doesn't implement a proxy. This is
  // to be maximally compatible with existing classes.
  if (vec_requires_fallback(x, info)) {
    if (info.type == vctrs_type_scalar) {
      vec_assert_vector(x, NULL);
    }

    if (is_compact(subscript)) {
      subscript = PROTECT_N(compact_materialize(subscript), &nprot);
    }

    SEXP out;

    if (has_dim(x)) {
      out = PROTECT_N(vec_slice_fallback(x, subscript), &nprot);
    } else {
      out = PROTECT_N(vec_slice_dispatch(x, subscript), &nprot);
    }

    // Take over attribute restoration only if there is no `[` method
    if (!vec_is_restored(out, x)) {
      out = vec_restore(out, x, restore_size, vec_owned(out));
    }

    UNPROTECT(nprot);
    return out;
  }

  switch (info.type) {
  case vctrs_type_null:
    r_stop_internal("vec_slice_impl", "Unexpected `NULL`.");

  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list: {
    SEXP out;

    if (has_dim(x)) {
      out = PROTECT_N(vec_slice_shaped(info.type, data, subscript), &nprot);

      SEXP names = PROTECT_N(Rf_getAttrib(x, R_DimNamesSymbol), &nprot);
      if (names != R_NilValue) {
        names = PROTECT_N(Rf_shallow_duplicate(names), &nprot);
        SEXP row_names = VECTOR_ELT(names, 0);
        row_names = PROTECT_N(slice_names(row_names, subscript), &nprot);
        SET_VECTOR_ELT(names, 0, row_names);
        Rf_setAttrib(out, R_DimNamesSymbol, names);
      }
    } else {
      out = PROTECT_N(vec_slice_base(info.type, data, subscript), &nprot);

      SEXP names = PROTECT_N(Rf_getAttrib(x, R_NamesSymbol), &nprot);
      names = PROTECT_N(slice_names(names, subscript), &nprot);
      Rf_setAttrib(out, R_NamesSymbol, names);
    }

    out = vec_restore(out, x, restore_size, vec_owned(out));

    UNPROTECT(nprot);
    return out;
  }

  case vctrs_type_dataframe: {
    SEXP out = PROTECT_N(df_slice(data, subscript), &nprot);
    out = vec_restore(out, x, restore_size, vec_owned(out));
    UNPROTECT(nprot);
    return out;
  }

  default:
    stop_unimplemented_vctrs_type("vec_slice_impl", info.type);
  }
}

bool vec_is_restored(SEXP x, SEXP to) {
  // Don't restore if there is an actual `[` method that ignored
  // attributes. Some methods like [.ts intentionally strip the class
  // and attributes. FIXME: This branch is now probably sufficient.
  if (s3_find_method("[", to, base_method_table) != R_NilValue) {
    return true;
  }

  SEXP attrib = ATTRIB(x);

  if (attrib == R_NilValue) {
    return false;
  }

  // Class is restored if it contains any other attributes than names.
  // We might want to add support for data frames later on.
  SEXP node = attrib;
  while (node != R_NilValue) {
    if (TAG(node) == R_NamesSymbol) {
      node = CDR(node);
      continue;
    }
    return true;
  }

  return false;
}


// [[ include("vctrs.h"); register() ]]
SEXP vec_slice(SEXP x, SEXP subscript) {
  vec_assert_vector(x, args_empty);

  subscript = PROTECT(vec_as_location(subscript, vec_size(x), PROTECT(vec_names(x))));
  SEXP out = vec_slice_impl(x, subscript);

  UNPROTECT(2);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_init(SEXP x, R_len_t n) {
  vec_assert_vector(x, NULL);

  if (n < 0) {
    r_abort("`n` must be a positive integer.");
  }

  SEXP i = PROTECT(compact_rep(NA_INTEGER, n));

  SEXP out = vec_slice_impl(x, i);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_init(SEXP x, SEXP n) {
  n = PROTECT(vec_cast(n, vctrs_shared_empty_int, args_n, args_empty));
  vec_assert_size(n, 1, args_n);
  R_len_t n_ = r_int_get(n, 0);

  SEXP out = vec_init(x, n_);

  UNPROTECT(1);
  return out;
}

// Exported for testing
// [[ register() ]]
SEXP vec_slice_seq(SEXP x, SEXP start, SEXP size, SEXP increasing) {
  R_len_t start_ = r_int_get(start, 0);
  R_len_t size_ = r_int_get(size, 0);
  bool increasing_ = r_lgl_get(increasing, 0);

  SEXP subscript = PROTECT(compact_seq(start_, size_, increasing_));
  SEXP out = vec_slice_impl(x, subscript);

  UNPROTECT(1);
  return out;
}

// Exported for testing
// [[ register() ]]
SEXP vec_slice_rep(SEXP x, SEXP i, SEXP n) {
  R_len_t i_ = r_int_get(i, 0);
  R_len_t n_ = r_int_get(n, 0);

  SEXP subscript = PROTECT(compact_rep(i_, n_));
  SEXP out = vec_slice_impl(x, subscript);

  UNPROTECT(1);
  return out;
}


void vctrs_init_slice(SEXP ns) {
  syms_vec_slice_fallback = Rf_install("vec_slice_fallback");
  syms_vec_slice_fallback_integer64 = Rf_install("vec_slice_fallback_integer64");
  syms_vec_slice_dispatch_integer64 = Rf_install("vec_slice_dispatch_integer64");

  fns_vec_slice_fallback = Rf_findVar(syms_vec_slice_fallback, ns);
  fns_vec_slice_fallback_integer64 = Rf_findVar(syms_vec_slice_fallback_integer64, ns);
  fns_vec_slice_dispatch_integer64 = Rf_findVar(syms_vec_slice_dispatch_integer64, ns);
}
