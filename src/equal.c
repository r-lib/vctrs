#include "vctrs.h"
#include <math.h>

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP ffi_vec_equal(
  SEXP ffi_x,
  SEXP ffi_y,
  SEXP ffi_na_equal,
  SEXP ffi_ptype,
  SEXP ffi_frame
) {
  struct r_lazy error_call = { .x = ffi_frame, .env = r_null };

  const bool na_equal = r_arg_as_bool(ffi_na_equal, "na_equal");

  return vec_equal(
    ffi_x,
    ffi_y,
    na_equal,
    ffi_ptype,
    vec_args.x,
    vec_args.y,
    error_call
  );
}

static inline SEXP lgl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP int_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP dbl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP cpl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP chr_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP raw_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP list_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP df_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);

SEXP vec_equal(
  SEXP x,
  SEXP y,
  bool na_equal,
  SEXP ptype,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy error_call
) {
  SEXP args = PROTECT(r_alloc_list(2));
  r_list_poke(args, 0, x);
  r_list_poke(args, 1, y);

  SEXP names = r_alloc_character(2);
  r_attrib_poke_names(args, names);
  SEXP x_name = PROTECT(vctrs_arg(p_x_arg));
  SEXP y_name = PROTECT(vctrs_arg(p_y_arg));
  r_chr_poke(names, 0, r_chr_get(x_name, 0));
  r_chr_poke(names, 1, r_chr_get(y_name, 0));

  args = PROTECT(vec_cast_common(args, ptype, vec_args.empty, error_call));

  const R_len_t size = vec_size_common(args, -1, vec_args.empty, error_call);

  x = r_list_get(args, 0);
  y = r_list_get(args, 1);

  const bool x_recycles = vec_size(x) == 1;
  const bool y_recycles = vec_size(y) == 1;

  SEXP x_proxy = PROTECT(vec_proxy_equal(x));
  SEXP y_proxy = PROTECT(vec_proxy_equal(y));

  x_proxy = PROTECT(vec_normalize_encoding(x_proxy));
  y_proxy = PROTECT(vec_normalize_encoding(y_proxy));

  enum vctrs_type type = vec_proxy_typeof(x_proxy);

  SEXP out;

  switch (type) {
  case VCTRS_TYPE_logical: out = lgl_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_integer: out = int_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_double: out = dbl_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_complex: out = cpl_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_character: out = chr_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_raw: out = raw_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_list: out = list_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_dataframe: out = df_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_scalar: r_abort_lazy_call(error_call, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", type);
  }

  UNPROTECT(8);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, X_I, Y_I) \
  SEXP out = PROTECT(r_new_logical(size));                                           \
  int* p_out = LOGICAL(out);                                                         \
                                                                                     \
  const CTYPE* p_x = CONST_DEREF(x);                                                 \
  const CTYPE* p_y = CONST_DEREF(y);                                                 \
                                                                                     \
  if (na_equal) {                                                                    \
    for (R_len_t i = 0; i < size; ++i) {                                             \
      p_out[i] = EQUAL_NA_EQUAL(p_x[X_I], p_y[Y_I]);                                 \
    }                                                                                \
  } else {                                                                           \
    for (R_len_t i = 0; i < size; ++i) {                                             \
      p_out[i] = EQUAL_NA_PROPAGATE(p_x[X_I], p_y[Y_I]);                             \
    }                                                                                \
  }                                                                                  \
                                                                                     \
  UNPROTECT(1);                                                                      \
  return out

#define EQUAL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE)            \
  if (x_recycles) {                                                              \
    if (y_recycles) {                                                            \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, 0, 0);  \
    } else {                                                                     \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, 0, i);  \
    }                                                                            \
  } else {                                                                       \
    if (y_recycles) {                                                            \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, i, 0);  \
    } else {                                                                     \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, i, i);  \
    }                                                                            \
  }


static inline
SEXP lgl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(int, LOGICAL_RO, lgl_equal_na_equal, lgl_equal_na_propagate);
}
static inline
SEXP int_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(int, INTEGER_RO, int_equal_na_equal, int_equal_na_propagate);
}
static inline
SEXP dbl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(double, REAL_RO, dbl_equal_na_equal, dbl_equal_na_propagate);
}
static inline
SEXP cpl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal, cpl_equal_na_propagate);
}
static inline
SEXP chr_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(SEXP, STRING_PTR_RO, chr_equal_na_equal, chr_equal_na_propagate);
}
static inline
SEXP raw_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(Rbyte, RAW_RO, raw_equal_na_equal, raw_equal_na_propagate);
}
static inline
SEXP list_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(SEXP, VECTOR_PTR_RO, list_equal_na_equal, list_equal_na_propagate);
}

#undef EQUAL
#undef EQUAL_IMPL

// -----------------------------------------------------------------------------

static
void vec_equal_col_na_equal(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
);

static
void vec_equal_col_na_propagate(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
);

static
SEXP df_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  int nprot = 0;

  SEXP out = PROTECT_N(r_new_logical(size), &nprot);
  int* p_out = LOGICAL(out);

  // Initialize to "equality" value
  // and only change if we learn that it differs
  for (R_len_t i = 0; i < size; ++i) {
    p_out[i] = 1;
  }

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  struct df_short_circuit_info* p_info = &info;
  PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, &nprot);

  R_len_t n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have the same number of columns");
  }

  void (*vec_equal_col)(SEXP, SEXP, bool, bool, int*, struct df_short_circuit_info*);

  if (na_equal) {
    vec_equal_col = vec_equal_col_na_equal;
  } else {
    vec_equal_col = vec_equal_col_na_propagate;
  }

  const SEXP* p_x = VECTOR_PTR_RO(x);
  const SEXP* p_y = VECTOR_PTR_RO(y);

  for (R_len_t i = 0; i < n_col; ++i) {
    vec_equal_col(p_x[i], p_y[i], x_recycles, y_recycles, p_out, p_info);

    if (p_info->remaining == 0) {
      break;
    }
  }

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, X_I, Y_I) do { \
  const CTYPE* p_x = CONST_DEREF(x);                             \
  const CTYPE* p_y = CONST_DEREF(y);                             \
                                                                 \
  for (R_len_t i = 0; i < p_info->size; ++i) {                   \
    if (p_info->p_row_known[i]) {                                \
      continue;                                                  \
    }                                                            \
                                                                 \
    int eq = EQUAL(p_x[X_I], p_y[Y_I]);                          \
                                                                 \
    if (eq <= 0) {                                               \
      p_out[i] = eq;                                             \
      p_info->p_row_known[i] = true;                             \
      --p_info->remaining;                                       \
                                                                 \
      if (p_info->remaining == 0) {                              \
        break;                                                   \
      }                                                          \
    }                                                            \
  }                                                              \
} while (0)

#define EQUAL_COL(CTYPE, CONST_DEREF, EQUAL)            \
  if (x_recycles) {                                     \
    if (y_recycles) {                                   \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, 0, 0);  \
    } else {                                            \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, 0, i);  \
    }                                                   \
  } else {                                              \
    if (y_recycles) {                                   \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, i, 0);  \
    } else {                                            \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, i, i);  \
    }                                                   \
  }

static
void vec_equal_col_na_equal(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: EQUAL_COL(int, LOGICAL_RO, lgl_equal_na_equal); break;
  case VCTRS_TYPE_integer: EQUAL_COL(int, INTEGER_RO, int_equal_na_equal); break;
  case VCTRS_TYPE_double: EQUAL_COL(double, REAL_RO, dbl_equal_na_equal); break;
  case VCTRS_TYPE_complex: EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal); break;
  case VCTRS_TYPE_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_na_equal); break;
  case VCTRS_TYPE_raw: EQUAL_COL(Rbyte, RAW_RO, raw_equal_na_equal); break;
  case VCTRS_TYPE_list: EQUAL_COL(SEXP, VECTOR_PTR_RO, list_equal_na_equal); break;
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened already.");
  case VCTRS_TYPE_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", vec_proxy_typeof(x));
  }
}

static
void vec_equal_col_na_propagate(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: EQUAL_COL(int, LOGICAL_RO, lgl_equal_na_propagate); break;
  case VCTRS_TYPE_integer: EQUAL_COL(int, INTEGER_RO, int_equal_na_propagate); break;
  case VCTRS_TYPE_double: EQUAL_COL(double, REAL_RO, dbl_equal_na_propagate); break;
  case VCTRS_TYPE_complex: EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_na_propagate); break;
  case VCTRS_TYPE_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_na_propagate); break;
  case VCTRS_TYPE_raw: EQUAL_COL(Rbyte, RAW_RO, raw_equal_na_propagate); break;
  case VCTRS_TYPE_list: EQUAL_COL(SEXP, VECTOR_PTR_RO, list_equal_na_propagate); break;
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened already.");
  case VCTRS_TYPE_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", vec_proxy_typeof(x));
  }
}

#undef EQUAL_COL
#undef EQUAL_COL_IMPL

// -----------------------------------------------------------------------------

// Missingness is never propagated through objects,
// so `na_equal` is always `true` in these macros

#define EQUAL_ALL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL)     \
  do {                                                    \
    const CTYPE* p_x = CONST_DEREF(x);                    \
    const CTYPE* p_y = CONST_DEREF(y);                    \
                                                          \
    for (R_len_t i = 0; i < n; ++i) {                     \
      if (!EQUAL_NA_EQUAL(p_x[i], p_y[i])) {              \
        return false;                                     \
      }                                                   \
    }                                                     \
    return true;                                          \
  }                                                       \
  while (0)

// Same as implementation for lists where we check length and attributes as
// well, but `VECTOR_PTR_RO()` doesn't support EXPRSXP, so we must use a
// separate loop that uses `VECTOR_ELT()` instead.
static inline
bool expr_equal_all(SEXP x, SEXP y, R_len_t n) {
  for (R_len_t i = 0; i < n; ++i) {
    SEXP x_elt = VECTOR_ELT(x, i);
    SEXP y_elt = VECTOR_ELT(y, i);

    if (!equal_object_normalized(x_elt, y_elt)) {
      return false;
    }
  }

  return true;
}

static inline bool vec_equal_attrib(SEXP x, SEXP y);

// [[ include("vctrs.h") ]]
bool equal_object(SEXP x, SEXP y) {
  x = PROTECT(vec_normalize_encoding(x));
  y = PROTECT(vec_normalize_encoding(y));

  bool out = equal_object_normalized(x, y);

  UNPROTECT(2);
  return out;
}

// Assumes `vec_normalize_encoding()` has already been called
// [[ include("vctrs.h") ]]
bool equal_object_normalized(SEXP x, SEXP y) {
  SEXPTYPE type = TYPEOF(x);

  if (type != TYPEOF(y)) {
    return false;
  }

  // Pointer comparison is all that is required for these types
  switch (type) {
  case NILSXP:
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP:
    return x == y;
  }

  // For other types, try a pointer comparison first before
  // performing an in depth equality check
  if (x == y) {
    return true;
  }

  switch (type) {
  // Handled below
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case STRSXP:
  case RAWSXP:
  case CPLXSXP:
  case EXPRSXP:
  case VECSXP: break;

  case DOTSXP:
  case LANGSXP:
  case LISTSXP:
  case BCODESXP: {
    if (!equal_object_normalized(ATTRIB(x), ATTRIB(y))) {
      return false;
    }

    if (!equal_object_normalized(CAR(x), CAR(y))) {
      return false;
    }

    x = CDR(x);
    y = CDR(y);

    if (!equal_object_normalized(x, y)) {
      return false;
    }

    return true;
  }

  case CLOSXP:
    if (!equal_object_normalized(ATTRIB(x), ATTRIB(y))) {
      return false;
    }
    if (!equal_object_normalized(BODY(x), BODY(y))) {
      return false;
    }
    if (!equal_object_normalized(CLOENV(x), CLOENV(y))) {
      return false;
    }
    if (!equal_object_normalized(FORMALS(x), FORMALS(y))) {
      return false;
    }
    return true;

  case NILSXP:
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP:
    // These are handled above with pointer comparison
    r_stop_internal("Unexpected reference type.");

  default:
    stop_unimplemented_type("equal_object_normalized", TYPEOF(x));
  }

  R_len_t n = Rf_length(x);
  if (n != Rf_length(y)) {
    return false;
  }

  if (!vec_equal_attrib(x, y)) {
    return false;
  }

  switch (type) {
  case LGLSXP:  EQUAL_ALL(int, LOGICAL_RO, lgl_equal_na_equal);
  case INTSXP:  EQUAL_ALL(int, INTEGER_RO, int_equal_na_equal);
  case REALSXP: EQUAL_ALL(double, REAL_RO, dbl_equal_na_equal);
  case STRSXP:  EQUAL_ALL(SEXP, STRING_PTR_RO, chr_equal_na_equal);
  case RAWSXP:  EQUAL_ALL(Rbyte, RAW_RO, raw_equal_na_equal);
  case CPLXSXP: EQUAL_ALL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal);
  case VECSXP:  EQUAL_ALL(SEXP, VECTOR_PTR_RO, list_equal_na_equal);

  // Want the length and attribute checks that are used on vectors,
  // but needs its own special iteration loop
  case EXPRSXP: return expr_equal_all(x, y, n);

  default: stop_unimplemented_type("equal_object", type);
  }
}

#undef EQUAL_ALL

// [[ register() ]]
SEXP vctrs_equal_object(SEXP x, SEXP y) {
  return Rf_ScalarLogical(equal_object(x, y));
}

// TODO: Sort attributes by tag before comparison

static inline bool vec_equal_attrib(SEXP x, SEXP y) {
  SEXP x_attrs = ATTRIB(x);
  SEXP y_attrs = ATTRIB(y);

  while (x_attrs != R_NilValue) {
    if (y_attrs == R_NilValue) {
      return false;
    }

    SEXP x_tag = TAG(x_attrs);
    SEXP y_tag = TAG(x_attrs);

    if (x_tag != y_tag) {
      return false;
    }

    if (!equal_object_normalized(CAR(x_attrs), CAR(y_attrs))) {
      return false;
    }

    x_attrs = CDR(x_attrs);
    y_attrs = CDR(y_attrs);
  }

  return true;
}


// [[ include("vctrs.h") ]]
bool equal_names(SEXP x, SEXP y) {
  SEXP x_names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  SEXP y_names = PROTECT(Rf_getAttrib(y, R_NamesSymbol));

  bool out = equal_object(x_names, y_names);

  UNPROTECT(2);
  return out;
}
