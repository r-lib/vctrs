#include <math.h>
#include "equal.h"
#include "vctrs.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static SEXP df_equal(SEXP x, SEXP y, bool na_equal, R_len_t size);

#define EQUAL(CTYPE, CONST_DEREF, SCALAR_EQUAL)          \
  do {                                                   \
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));    \
    int* p_out = LOGICAL(out);                           \
                                                         \
    const CTYPE* p_x = CONST_DEREF(x);                   \
    const CTYPE* p_y = CONST_DEREF(y);                   \
                                                         \
    for (R_len_t i = 0; i < size; ++i) {                 \
      p_out[i] = SCALAR_EQUAL(p_x[i], p_y[i], na_equal); \
    }                                                    \
                                                         \
    UNPROTECT(3);                                        \
    return out;                                          \
  }                                                      \
  while (0)

// [[ register() ]]
SEXP vctrs_equal(SEXP x, SEXP y, SEXP na_equal_) {
  x = PROTECT(vec_proxy_equal(x));
  y = PROTECT(vec_proxy_equal(y));

  R_len_t size = vec_size(x);

  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || size != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = r_bool_as_int(na_equal_);

  switch (type) {
  case vctrs_type_logical:   EQUAL(int, LOGICAL_RO, lgl_equal_scalar);
  case vctrs_type_integer:   EQUAL(int, INTEGER_RO, int_equal_scalar);
  case vctrs_type_double:    EQUAL(double, REAL_RO, dbl_equal_scalar);
  case vctrs_type_raw:       EQUAL(Rbyte, RAW_RO, raw_equal_scalar);
  case vctrs_type_complex:   EQUAL(Rcomplex, COMPLEX_RO, cpl_equal_scalar);
  case vctrs_type_character: EQUAL(SEXP, STRING_PTR_RO, chr_equal_scalar);
  case vctrs_type_list:      EQUAL(SEXP, VECTOR_PTR_RO, list_equal_scalar);
  case vctrs_type_dataframe: {
    SEXP out = PROTECT(df_equal(x, y, na_equal, size));
    UNPROTECT(3);
    return out;
  }
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_equal()`");
  default:                   Rf_error("Unimplemented type in `vctrs_equal()`");
  }
}

#undef EQUAL

// -----------------------------------------------------------------------------

// Missingness is never propagated through objects,
// so `na_equal` is always `true` in these macros

#define EQUAL_ALL(CTYPE, CONST_DEREF, SCALAR_EQUAL)       \
  do {                                                    \
    const CTYPE* p_x = CONST_DEREF(x);                    \
    const CTYPE* p_y = CONST_DEREF(y);                    \
                                                          \
    for (R_len_t i = 0; i < n; ++i) {                     \
      if (!SCALAR_EQUAL(p_x[i], p_y[i])) {                \
        return false;                                     \
      }                                                   \
    }                                                     \
    return true;                                          \
  }                                                       \
  while (0)

static inline bool vec_equal_attrib(SEXP x, SEXP y);

// [[ include("vctrs.h") ]]
bool equal_object(SEXP x, SEXP y) {
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
    if (!equal_object(ATTRIB(x), ATTRIB(y))) {
      return false;
    }

    if (!equal_object(CAR(x), CAR(y))) {
      return false;
    }

    x = CDR(x);
    y = CDR(y);

    if (!equal_object(x, y)) {
      return false;
    }

    return true;
  }

  case CLOSXP:
    if (!equal_object(ATTRIB(x), ATTRIB(y))) {
      return false;
    }
    if (!equal_object(BODY(x), BODY(y))) {
      return false;
    }
    if (!equal_object(CLOENV(x), CLOENV(y))) {
      return false;
    }
    if (!equal_object(FORMALS(x), FORMALS(y))) {
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
    stop_internal("equal_object", "Unexpected reference type.");

  default:
    stop_unimplemented_type("equal_object", TYPEOF(x));
  }

  R_len_t n = Rf_length(x);
  if (n != Rf_length(y)) {
    return false;
  }

  if (!vec_equal_attrib(x, y)) {
    return false;
  }

  switch (type) {
  case LGLSXP:  EQUAL_ALL(int, LOGICAL_RO, lgl_equal_scalar_na_equal);
  case INTSXP:  EQUAL_ALL(int, INTEGER_RO, int_equal_scalar_na_equal);
  case REALSXP: EQUAL_ALL(double, REAL_RO, dbl_equal_scalar_na_equal);
  case STRSXP:  EQUAL_ALL(SEXP, STRING_PTR_RO, chr_equal_scalar_na_equal);
  case RAWSXP:  EQUAL_ALL(Rbyte, RAW_RO, raw_equal_scalar_na_equal);
  case CPLXSXP: EQUAL_ALL(Rcomplex, COMPLEX_RO, cpl_equal_scalar_na_equal);
  case EXPRSXP:
  case VECSXP:  EQUAL_ALL(SEXP, VECTOR_PTR_RO, list_equal_scalar_na_equal);
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

    if (!equal_object(CAR(x_attrs), CAR(y_attrs))) {
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

static void vec_equal_col(int* p_out,
                          struct df_short_circuit_info* p_info,
                          SEXP x,
                          SEXP y,
                          bool na_equal);

static void df_equal_impl(int* p_out,
                          struct df_short_circuit_info* p_info,
                          SEXP x,
                          SEXP y,
                          bool na_equal) {
  int n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have the same number of columns");
  }

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP x_col = VECTOR_ELT(x, i);
    SEXP y_col = VECTOR_ELT(y, i);

    vec_equal_col(p_out, p_info, x_col, y_col, na_equal);

    // If we know all comparison values, break
    if (p_info->remaining == 0) {
      break;
    }
  }
}

static SEXP df_equal(SEXP x, SEXP y, bool na_equal, R_len_t size) {
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

  df_equal_impl(p_out, p_info, x, y, na_equal);

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL_COL(CTYPE, CONST_DEREF, SCALAR_EQUAL)                  \
do {                                                                 \
  const CTYPE* p_x = CONST_DEREF(x);                                 \
  const CTYPE* p_y = CONST_DEREF(y);                                 \
                                                                     \
  for (R_len_t i = 0; i < p_info->size; ++i) {                       \
    if (p_info->p_row_known[i]) {                                    \
      continue;                                                      \
    }                                                                \
                                                                     \
    int eq = SCALAR_EQUAL(p_x[i], p_y[i], na_equal);                 \
                                                                     \
    if (eq <= 0) {                                                   \
      p_out[i] = eq;                                                 \
      p_info->p_row_known[i] = true;                                 \
      --p_info->remaining;                                           \
                                                                     \
      if (p_info->remaining == 0) {                                  \
        break;                                                       \
      }                                                              \
    }                                                                \
  }                                                                  \
}                                                                    \
while (0)

static void vec_equal_col(int* p_out,
                          struct df_short_circuit_info* p_info,
                          SEXP x,
                          SEXP y,
                          bool na_equal) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical:   EQUAL_COL(int, LOGICAL_RO, lgl_equal_scalar); break;
  case vctrs_type_integer:   EQUAL_COL(int, INTEGER_RO, int_equal_scalar); break;
  case vctrs_type_double:    EQUAL_COL(double, REAL_RO, dbl_equal_scalar); break;
  case vctrs_type_raw:       EQUAL_COL(Rbyte, RAW_RO, raw_equal_scalar); break;
  case vctrs_type_complex:   EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_scalar); break;
  case vctrs_type_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_scalar); break;
  case vctrs_type_list:      EQUAL_COL(SEXP, VECTOR_PTR_RO, list_equal_scalar); break;
  case vctrs_type_dataframe: df_equal_impl(p_out, p_info, x, y, na_equal); break;
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_equal()`");
  default:                   Rf_error("Unimplemented type in `vctrs_equal()`");
  }
}

#undef EQUAL_COL

// -----------------------------------------------------------------------------

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

// [[ register() ]]
SEXP vctrs_equal_na(SEXP x) {
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
