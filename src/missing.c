#include "vctrs.h"

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

// [[ include("missing.h") ]]
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
