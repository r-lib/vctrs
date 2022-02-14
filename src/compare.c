#include "vctrs.h"
#include "utils.h"
#include "compare.h"
#include "translate.h"
#include <strings.h>

static void stop_not_comparable(SEXP x, SEXP y, const char* message) {
  r_abort("`x` and `y` are not comparable: %s", message);
}

// -----------------------------------------------------------------------------

static SEXP df_compare(SEXP x, SEXP y, bool na_equal, R_len_t size);

#define COMPARE(CTYPE, CONST_DEREF, SCALAR_COMPARE)     \
do {                                                    \
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));     \
  int* p_out = INTEGER(out);                            \
                                                        \
  const CTYPE* p_x = CONST_DEREF(x);                    \
  const CTYPE* p_y = CONST_DEREF(y);                    \
                                                        \
  for (R_len_t i = 0; i < size; ++i) {                  \
    p_out[i] = SCALAR_COMPARE(p_x[i], p_y[i]);          \
  }                                                     \
                                                        \
  UNPROTECT(3);                                         \
  return out;                                           \
}                                                       \
while (0)

// [[ include("compare.h") ]]
SEXP vec_compare(SEXP x, SEXP y, bool na_equal) {
  R_len_t size = vec_size(x);

  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || size != vec_size(y)) {
    stop_not_comparable(x, y, "must have the same types and lengths");
  }

  x = PROTECT(vec_normalize_encoding(x));
  y = PROTECT(vec_normalize_encoding(y));

  if (type == vctrs_type_dataframe) {
    SEXP out = df_compare(x, y, na_equal, size);
    UNPROTECT(2);
    return out;
  }

  if (na_equal) {
    switch (type) {
    case vctrs_type_logical:   COMPARE(int, LOGICAL_RO, lgl_compare_na_equal);
    case vctrs_type_integer:   COMPARE(int, INTEGER_RO, int_compare_na_equal);
    case vctrs_type_double:    COMPARE(double, REAL_RO, dbl_compare_na_equal);
    case vctrs_type_character: COMPARE(SEXP, STRING_PTR_RO, chr_compare_na_equal);
    case vctrs_type_scalar:    r_abort("Can't compare scalars with `vec_compare()`");
    case vctrs_type_list:      r_abort("Can't compare lists with `vec_compare()`");
    default:                   stop_unimplemented_vctrs_type("vec_compare", type);
    }
  } else {
    switch (type) {
    case vctrs_type_logical:   COMPARE(int, LOGICAL_RO, lgl_compare_na_propagate);
    case vctrs_type_integer:   COMPARE(int, INTEGER_RO, int_compare_na_propagate);
    case vctrs_type_double:    COMPARE(double, REAL_RO, dbl_compare_na_propagate);
    case vctrs_type_character: COMPARE(SEXP, STRING_PTR_RO, chr_compare_na_propagate);
    case vctrs_type_scalar:    r_abort("Can't compare scalars with `vec_compare()`");
    case vctrs_type_list:      r_abort("Can't compare lists with `vec_compare()`");
    default:                   stop_unimplemented_vctrs_type("vec_compare", type);
    }
  }
}

#undef COMPARE

// [[ register() ]]
SEXP vctrs_compare(SEXP x, SEXP y, SEXP na_equal) {
  const bool c_na_equal = r_bool_as_int(na_equal);
  return vec_compare(x, y, c_na_equal);
}

// -----------------------------------------------------------------------------

static void vec_compare_col(int* p_out,
                            struct df_short_circuit_info* p_info,
                            SEXP x,
                            SEXP y,
                            bool na_equal);

static void df_compare_impl(int* p_out,
                            struct df_short_circuit_info* p_info,
                            SEXP x,
                            SEXP y,
                            bool na_equal);

static SEXP df_compare(SEXP x, SEXP y, bool na_equal, R_len_t size) {
  int nprot = 0;

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, size), &nprot);
  int* p_out = INTEGER(out);

  // Initialize to "equality" value and only change if we learn that it differs.
  // This also determines the zero column result.
  memset(p_out, 0, size * sizeof(int));

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  struct df_short_circuit_info* p_info = &info;
  PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, &nprot);

  df_compare_impl(p_out, p_info, x, y, na_equal);

  UNPROTECT(nprot);
  return out;
}

static void df_compare_impl(int* p_out,
                            struct df_short_circuit_info* p_info,
                            SEXP x,
                            SEXP y,
                            bool na_equal) {
  int n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    stop_not_comparable(x, y, "must have the same number of columns");
  }

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP x_col = VECTOR_ELT(x, i);
    SEXP y_col = VECTOR_ELT(y, i);

    vec_compare_col(p_out, p_info, x_col, y_col, na_equal);

    // If we know all comparison values, break
    if (p_info->remaining == 0) {
      break;
    }
  }
}

// -----------------------------------------------------------------------------

#define COMPARE_COL(CTYPE, CONST_DEREF, SCALAR_COMPARE) \
do {                                                    \
  const CTYPE* p_x = CONST_DEREF(x);                    \
  const CTYPE* p_y = CONST_DEREF(y);                    \
                                                        \
  for (R_len_t i = 0; i < p_info->size; ++i) {          \
    if (p_info->p_row_known[i]) {                       \
      continue;                                         \
    }                                                   \
                                                        \
    int cmp = SCALAR_COMPARE(p_x[i], p_y[i]);           \
                                                        \
    if (cmp != 0) {                                     \
      p_out[i] = cmp;                                   \
      p_info->p_row_known[i] = true;                    \
      --p_info->remaining;                              \
                                                        \
      if (p_info->remaining == 0) {                     \
        break;                                          \
      }                                                 \
    }                                                   \
  }                                                     \
}                                                       \
while (0)

static void vec_compare_col(int* p_out,
                            struct df_short_circuit_info* p_info,
                            SEXP x,
                            SEXP y,
                            bool na_equal) {
  enum vctrs_type type = vec_proxy_typeof(x);

  if (type == vctrs_type_dataframe) {
    df_compare_impl(p_out, p_info, x, y, na_equal);
    return;
  }

  if (na_equal) {
    switch (type) {
    case vctrs_type_logical:   COMPARE_COL(int, LOGICAL_RO, lgl_compare_na_equal); break;
    case vctrs_type_integer:   COMPARE_COL(int, INTEGER_RO, int_compare_na_equal); break;
    case vctrs_type_double:    COMPARE_COL(double, REAL_RO, dbl_compare_na_equal); break;
    case vctrs_type_character: COMPARE_COL(SEXP, STRING_PTR_RO, chr_compare_na_equal); break;
    case vctrs_type_scalar:    r_abort("Can't compare scalars with `vctrs_compare()`");
    case vctrs_type_list:      r_abort("Can't compare lists with `vctrs_compare()`");
    default:                   stop_unimplemented_vctrs_type("vec_compare_col", type);
    }
  } else {
    switch (type) {
    case vctrs_type_logical:   COMPARE_COL(int, LOGICAL_RO, lgl_compare_na_propagate); break;
    case vctrs_type_integer:   COMPARE_COL(int, INTEGER_RO, int_compare_na_propagate); break;
    case vctrs_type_double:    COMPARE_COL(double, REAL_RO, dbl_compare_na_propagate); break;
    case vctrs_type_character: COMPARE_COL(SEXP, STRING_PTR_RO, chr_compare_na_propagate); break;
    case vctrs_type_scalar:    r_abort("Can't compare scalars with `vctrs_compare()`");
    case vctrs_type_list:      r_abort("Can't compare lists with `vctrs_compare()`");
    default:                   stop_unimplemented_vctrs_type("vec_compare_col", type);
    }
  }
}

#undef COMPARE_COL
