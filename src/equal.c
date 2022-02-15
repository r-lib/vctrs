#include "vctrs.h"
#include <math.h>

// -----------------------------------------------------------------------------

static SEXP vec_equal(SEXP x, SEXP y, bool na_equal);

// [[ register() ]]
SEXP vctrs_equal(SEXP x, SEXP y, SEXP na_equal) {
  bool c_na_equal = r_bool_as_int(na_equal);
  return vec_equal(x, y, c_na_equal);
}

static SEXP lgl_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);
static SEXP int_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);
static SEXP dbl_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);
static SEXP cpl_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);
static SEXP chr_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);
static SEXP raw_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);
static SEXP list_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);
static SEXP df_equal(SEXP x, SEXP y, R_len_t size, bool na_equal);

/*
 * Recycling and casting is done at the R level
 */
static
SEXP vec_equal(SEXP x, SEXP y, bool na_equal) {
  SEXP x_proxy = PROTECT(vec_proxy_equal(x));
  SEXP y_proxy = PROTECT(vec_proxy_equal(y));

  x_proxy = PROTECT(vec_normalize_encoding(x_proxy));
  y_proxy = PROTECT(vec_normalize_encoding(y_proxy));

  R_len_t size = vec_size(x_proxy);
  enum vctrs_type type = vec_proxy_typeof(x_proxy);

  if (type != vec_proxy_typeof(y_proxy) || size != vec_size(y_proxy)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths.");
  }

  SEXP out;

  switch (type) {
  case vctrs_type_logical: out = lgl_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_integer: out = int_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_double: out = dbl_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_complex: out = cpl_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_character: out = chr_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_raw: out = raw_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_list: out = list_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_dataframe: out = df_equal(x_proxy, y_proxy, size, na_equal); break;
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", type);
  }

  UNPROTECT(4);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE) \
  SEXP out = PROTECT(r_new_logical(size));                            \
  int* p_out = LOGICAL(out);                                          \
                                                                      \
  const CTYPE* p_x = CONST_DEREF(x);                                  \
  const CTYPE* p_y = CONST_DEREF(y);                                  \
                                                                      \
  if (na_equal) {                                                     \
    for (R_len_t i = 0; i < size; ++i) {                              \
      p_out[i] = EQUAL_NA_EQUAL(p_x[i], p_y[i]);                      \
    }                                                                 \
  } else {                                                            \
    for (R_len_t i = 0; i < size; ++i) {                              \
      p_out[i] = EQUAL_NA_PROPAGATE(p_x[i], p_y[i]);                  \
    }                                                                 \
  }                                                                   \
                                                                      \
  UNPROTECT(1);                                                       \
  return out;


static
SEXP lgl_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
  EQUAL(int, LOGICAL_RO, lgl_equal_na_equal, lgl_equal_na_propagate);
}
static
SEXP int_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
  EQUAL(int, INTEGER_RO, int_equal_na_equal, int_equal_na_propagate);
}
static
SEXP dbl_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
  EQUAL(double, REAL_RO, dbl_equal_na_equal, dbl_equal_na_propagate);
}
static
SEXP cpl_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
  EQUAL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal, cpl_equal_na_propagate);
}
static
SEXP chr_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
  EQUAL(SEXP, STRING_PTR_RO, chr_equal_na_equal, chr_equal_na_propagate);
}
static
SEXP raw_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
  EQUAL(Rbyte, RAW_RO, raw_equal_na_equal, raw_equal_na_propagate);
}
static
SEXP list_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
  EQUAL(SEXP, VECTOR_PTR_RO, list_equal_na_equal, list_equal_na_propagate);
}

#undef EQUAL

// -----------------------------------------------------------------------------

static void vec_equal_col_na_equal(SEXP x,
                                   SEXP y,
                                   int* p_out,
                                   struct df_short_circuit_info* p_info);

static void vec_equal_col_na_propagate(SEXP x,
                                       SEXP y,
                                       int* p_out,
                                       struct df_short_circuit_info* p_info);

static
SEXP df_equal(SEXP x, SEXP y, R_len_t size, bool na_equal) {
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

  void (*vec_equal_col)(SEXP, SEXP, int*, struct df_short_circuit_info*);

  if (na_equal) {
    vec_equal_col = vec_equal_col_na_equal;
  } else {
    vec_equal_col = vec_equal_col_na_propagate;
  }

  const SEXP* p_x = VECTOR_PTR_RO(x);
  const SEXP* p_y = VECTOR_PTR_RO(y);

  for (R_len_t i = 0; i < n_col; ++i) {
    vec_equal_col(p_x[i], p_y[i], p_out, p_info);

    if (p_info->remaining == 0) {
      break;
    }
  }

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL_COL(CTYPE, CONST_DEREF, EQUAL) do { \
  const CTYPE* p_x = CONST_DEREF(x);              \
  const CTYPE* p_y = CONST_DEREF(y);              \
                                                  \
  for (R_len_t i = 0; i < p_info->size; ++i) {    \
    if (p_info->p_row_known[i]) {                 \
      continue;                                   \
    }                                             \
                                                  \
    int eq = EQUAL(p_x[i], p_y[i]);               \
                                                  \
    if (eq <= 0) {                                \
      p_out[i] = eq;                              \
      p_info->p_row_known[i] = true;              \
      --p_info->remaining;                        \
                                                  \
      if (p_info->remaining == 0) {               \
        break;                                    \
      }                                           \
    }                                             \
  }                                               \
} while (0)

static
void vec_equal_col_na_equal(SEXP x,
                            SEXP y,
                            int* p_out,
                            struct df_short_circuit_info* p_info) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: EQUAL_COL(int, LOGICAL_RO, lgl_equal_na_equal); break;
  case vctrs_type_integer: EQUAL_COL(int, INTEGER_RO, int_equal_na_equal); break;
  case vctrs_type_double: EQUAL_COL(double, REAL_RO, dbl_equal_na_equal); break;
  case vctrs_type_complex: EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal); break;
  case vctrs_type_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_na_equal); break;
  case vctrs_type_raw: EQUAL_COL(Rbyte, RAW_RO, raw_equal_na_equal); break;
  case vctrs_type_list: EQUAL_COL(SEXP, VECTOR_PTR_RO, list_equal_na_equal); break;
  case vctrs_type_dataframe: r_stop_internal("Data frame columns should be flattened already.");
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", vec_proxy_typeof(x));
  }
}

static
void vec_equal_col_na_propagate(SEXP x,
                                SEXP y,
                                int* p_out,
                                struct df_short_circuit_info* p_info) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: EQUAL_COL(int, LOGICAL_RO, lgl_equal_na_propagate); break;
  case vctrs_type_integer: EQUAL_COL(int, INTEGER_RO, int_equal_na_propagate); break;
  case vctrs_type_double: EQUAL_COL(double, REAL_RO, dbl_equal_na_propagate); break;
  case vctrs_type_complex: EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_na_propagate); break;
  case vctrs_type_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_na_propagate); break;
  case vctrs_type_raw: EQUAL_COL(Rbyte, RAW_RO, raw_equal_na_propagate); break;
  case vctrs_type_list: EQUAL_COL(SEXP, VECTOR_PTR_RO, list_equal_na_propagate); break;
  case vctrs_type_dataframe: r_stop_internal("Data frame columns should be flattened already.");
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", vec_proxy_typeof(x));
  }
}

#undef EQUAL_COL

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
  case EXPRSXP:
  case VECSXP:  EQUAL_ALL(SEXP, VECTOR_PTR_RO, list_equal_na_equal);
  default:      stop_unimplemented_type("equal_object", type);
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

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_equal_na(SEXP x) {
  return vec_equal_na(x);
}

#define EQUAL_NA(CTYPE, CONST_DEREF, IS_MISSING)           \
  do {                                                     \
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));      \
    int* p_out = LOGICAL(out);                             \
                                                           \
    const CTYPE* p_x = CONST_DEREF(x);                     \
                                                           \
    for (R_len_t i = 0; i < size; ++i) {                   \
      p_out[i] = IS_MISSING(p_x[i]);                       \
    }                                                      \
                                                           \
    UNPROTECT(2);                                          \
    return out;                                            \
  }                                                        \
  while (0)

static SEXP df_equal_na(SEXP x, R_len_t size);

// [[ include("equal.h") ]]
SEXP vec_equal_na(SEXP x) {
  R_len_t size = vec_size(x);

  x = PROTECT(vec_proxy_equal(x));

  enum vctrs_type type = vec_proxy_typeof(x);

  switch (type) {
  case vctrs_type_logical:   EQUAL_NA(int, LOGICAL_RO, lgl_is_missing);
  case vctrs_type_integer:   EQUAL_NA(int, INTEGER_RO, int_is_missing);
  case vctrs_type_double:    EQUAL_NA(double, REAL_RO, dbl_is_missing);
  case vctrs_type_complex:   EQUAL_NA(Rcomplex, COMPLEX_RO, cpl_is_missing);
  case vctrs_type_raw:       EQUAL_NA(Rbyte, RAW_RO, raw_is_missing);
  case vctrs_type_character: EQUAL_NA(SEXP, STRING_PTR_RO, chr_is_missing);
  case vctrs_type_list:      EQUAL_NA(SEXP, VECTOR_PTR_RO, list_is_missing);
  case vctrs_type_dataframe: {
    SEXP out = df_equal_na(x, size);
    UNPROTECT(1);
    return out;
  }
  case vctrs_type_null: {
    UNPROTECT(1);
    return vctrs_shared_empty_lgl;
  }
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't detect `NA` values in scalars with `vctrs_equal_na()`.");
  default:                   Rf_error("Unimplemented type in `vctrs_equal_na()`.");
  }
}

#undef EQUAL_NA

// -----------------------------------------------------------------------------

static void vec_equal_na_col(int* p_out,
                             struct df_short_circuit_info* p_info,
                             SEXP x);

static void df_equal_na_impl(int* p_out,
                             struct df_short_circuit_info* p_info,
                             SEXP x) {
  int n_col = Rf_length(x);

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(x, i);

    vec_equal_na_col(p_out, p_info, col);

    // If all rows have at least one non-missing value, break
    if (p_info->remaining == 0) {
      break;
    }
  }
}

static SEXP df_equal_na(SEXP x, R_len_t size) {
  int nprot = 0;

  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, size), &nprot);
  int* p_out = LOGICAL(out);

  // Initialize to "equality" value
  // and only change if we learn that it differs
  for (R_len_t i = 0; i < size; ++i) {
    p_out[i] = 1;
  }

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  struct df_short_circuit_info* p_info = &info;
  PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, &nprot);

  df_equal_na_impl(p_out, p_info, x);

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL_NA_COL(CTYPE, CONST_DEREF, IS_MISSING)           \
do {                                                           \
  const CTYPE* p_x = CONST_DEREF(x);                           \
                                                               \
  for (R_len_t i = 0; i < p_info->size; ++i) {                 \
    if (p_info->p_row_known[i]) {                              \
      continue;                                                \
    }                                                          \
                                                               \
    if (!IS_MISSING(p_x[i])) {                                 \
      p_out[i] = 0;                                            \
      p_info->p_row_known[i] = true;                           \
      --p_info->remaining;                                     \
                                                               \
      if (p_info->remaining == 0) {                            \
        break;                                                 \
      }                                                        \
    }                                                          \
  }                                                            \
}                                                              \
while (0)

static void vec_equal_na_col(int* p_out,
                             struct df_short_circuit_info* p_info,
                             SEXP x) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical:   EQUAL_NA_COL(int, LOGICAL_RO, lgl_is_missing); break;
  case vctrs_type_integer:   EQUAL_NA_COL(int, INTEGER_RO, int_is_missing); break;
  case vctrs_type_double:    EQUAL_NA_COL(double, REAL_RO, dbl_is_missing); break;
  case vctrs_type_complex:   EQUAL_NA_COL(Rcomplex, COMPLEX_RO, cpl_is_missing); break;
  case vctrs_type_raw:       EQUAL_NA_COL(Rbyte, RAW_RO, raw_is_missing); break;
  case vctrs_type_character: EQUAL_NA_COL(SEXP, STRING_PTR_RO, chr_is_missing); break;
  case vctrs_type_list:      EQUAL_NA_COL(SEXP, VECTOR_PTR_RO, list_is_missing); break;
  case vctrs_type_dataframe: df_equal_na_impl(p_out, p_info, x); break;
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal_na()`");
  default:                   Rf_error("Unimplemented type in `vec_equal_na()`");
  }
}

#undef EQUAL_NA_COL
