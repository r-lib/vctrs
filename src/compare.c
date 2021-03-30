#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "translate.h"
#include <strings.h>

static void stop_not_comparable(SEXP x, SEXP y, const char* message) {
  Rf_errorcall(R_NilValue, "`x` and `y` are not comparable: %s", message);
}

// https://stackoverflow.com/questions/10996418
static inline int icmp(int x, int y) {
  return (x > y) - (x < y);
}
int qsort_icmp(const void* x, const void* y) {
  return icmp(*((int*) x), *((int*) y));
}

static int dcmp(double x, double y) {
  return (x > y) - (x < y);
}

// Assume translation handled by `vec_normalize_encoding()`
static inline int scmp(SEXP x, SEXP y) {
  if (x == y) {
    return 0;
  }

  int cmp = strcmp(CHAR(x), CHAR(y));
  return cmp / abs(cmp);
}

// -----------------------------------------------------------------------------

static inline int lgl_compare_scalar(const int* x, const int* y, bool na_equal) {
  int xi = *x;
  int yj = *y;

  if (na_equal) {
    return icmp(xi, yj);
  } else {
    return (xi == NA_LOGICAL || yj == NA_LOGICAL) ? NA_INTEGER : icmp(xi, yj);
  }
}

static inline int int_compare_scalar(const int* x, const int* y, bool na_equal) {
  int xi = *x;
  int yj = *y;

  if (na_equal) {
    return icmp(xi, yj);
  } else {
    return (xi == NA_INTEGER || yj == NA_INTEGER) ? NA_INTEGER : icmp(xi, yj);
  }
}

static inline int dbl_compare_scalar(const double* x, const double* y, bool na_equal) {
  double xi = *x;
  double yj = *y;

  if (na_equal) {
    enum vctrs_dbl_class x_class = dbl_classify(xi);
    enum vctrs_dbl_class y_class = dbl_classify(yj);

    switch (x_class) {
    case vctrs_dbl_number: {
      switch (y_class) {
      case vctrs_dbl_number: return dcmp(xi, yj);
      case vctrs_dbl_missing: return 1;
      case vctrs_dbl_nan: return 1;
      }
    }
    case vctrs_dbl_missing: {
      switch (y_class) {
      case vctrs_dbl_number: return -1;
      case vctrs_dbl_missing: return 0;
      case vctrs_dbl_nan: return 1;
      }
    }
    case vctrs_dbl_nan: {
      switch (y_class) {
      case vctrs_dbl_number: return -1;
      case vctrs_dbl_missing: return -1;
      case vctrs_dbl_nan: return 0;
      }
    }
    }
  } else {
    return (isnan(xi) || isnan(yj)) ? NA_INTEGER : dcmp(xi, yj);
  }

  never_reached("dbl_compare_scalar");
}

static inline int chr_compare_scalar(const SEXP* x, const SEXP* y, bool na_equal) {
  const SEXP xi = *x;
  const SEXP yj = *y;

  if (na_equal) {
    if (xi == NA_STRING) {
      return (yj == NA_STRING) ? 0 : -1;
    } else {
      return (yj == NA_STRING) ? 1 : scmp(xi, yj);
    }
  } else {
    return (xi == NA_STRING || yj == NA_STRING) ? NA_INTEGER : scmp(xi, yj);
  }
}

static inline int df_compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal, int n_col) {
  int cmp;

  for (int k = 0; k < n_col; ++k) {
    SEXP col_x = VECTOR_ELT(x, k);
    SEXP col_y = VECTOR_ELT(y, k);

    cmp = compare_scalar(col_x, i, col_y, j, na_equal);

    if (cmp != 0) {
      return cmp;
    }
  }

  return cmp;
}

// -----------------------------------------------------------------------------

// [[ include("vctrs.h") ]]
int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_compare_scalar(LOGICAL_RO(x) + i, LOGICAL_RO(y) + j, na_equal);
  case INTSXP: return int_compare_scalar(INTEGER_RO(x) + i, INTEGER_RO(y) + j, na_equal);
  case REALSXP: return dbl_compare_scalar(REAL_RO(x) + i, REAL_RO(y) + j, na_equal);
  case STRSXP: return chr_compare_scalar(STRING_PTR_RO(x) + i, STRING_PTR_RO(y) + j, na_equal);
  default: break;
  }

  switch (vec_proxy_typeof(x)) {
  case vctrs_type_list: stop_not_comparable(x, y, "lists are not comparable");
  case vctrs_type_dataframe: {
    int n_col = Rf_length(x);

    if (n_col != Rf_length(y)) {
      stop_not_comparable(x, y, "must have the same number of columns");
    }

    if (n_col == 0) {
      stop_not_comparable(x, y, "data frame with zero columns");
    }

    return df_compare_scalar(x, i, y, j, na_equal, n_col);
  }
  default: break;
  }

  Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
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
  for (R_len_t i = 0; i < size; ++i, ++p_x, ++p_y) {    \
    p_out[i] = SCALAR_COMPARE(p_x, p_y, na_equal);      \
  }                                                     \
                                                        \
  UNPROTECT(3);                                         \
  return out;                                           \
}                                                       \
while (0)

// [[ register() ]]
SEXP vctrs_compare(SEXP x, SEXP y, SEXP na_equal_) {
  bool na_equal = r_bool_as_int(na_equal_);

  R_len_t size = vec_size(x);

  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || size != vec_size(y)) {
    stop_not_comparable(x, y, "must have the same types and lengths");
  }

  x = PROTECT(vec_normalize_encoding(x));
  y = PROTECT(vec_normalize_encoding(y));

  switch (type) {
  case vctrs_type_logical:   COMPARE(int, LOGICAL_RO, lgl_compare_scalar);
  case vctrs_type_integer:   COMPARE(int, INTEGER_RO, int_compare_scalar);
  case vctrs_type_double:    COMPARE(double, REAL_RO, dbl_compare_scalar);
  case vctrs_type_character: COMPARE(SEXP, STRING_PTR_RO, chr_compare_scalar);
  case vctrs_type_dataframe: {
    SEXP out = df_compare(x, y, na_equal, size);
    UNPROTECT(2);
    return out;
  }
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_compare()`");
  case vctrs_type_list:      Rf_errorcall(R_NilValue, "Can't compare lists with `vctrs_compare()`");
  default:                   Rf_error("Unimplemented type in `vctrs_compare()`");
  }
}

#undef COMPARE

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

  // Initialize to "equality" value
  // and only change if we learn that it differs
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

  if (n_col == 0) {
    stop_not_comparable(x, y, "data frame with zero columns");
  }

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

#define COMPARE_COL(CTYPE, CONST_DEREF, SCALAR_COMPARE)              \
do {                                                                 \
  const CTYPE* p_x = CONST_DEREF(x);                                 \
  const CTYPE* p_y = CONST_DEREF(y);                                 \
                                                                     \
  for (R_len_t i = 0; i < p_info->size; ++i, ++p_x, ++p_y) {         \
    if (p_info->p_row_known[i]) {                                    \
      continue;                                                      \
    }                                                                \
                                                                     \
    int cmp = SCALAR_COMPARE(p_x, p_y, na_equal);                    \
                                                                     \
    if (cmp != 0) {                                                  \
      p_out[i] = cmp;                                                \
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

static void vec_compare_col(int* p_out,
                            struct df_short_circuit_info* p_info,
                            SEXP x,
                            SEXP y,
                            bool na_equal) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical:   COMPARE_COL(int, LOGICAL_RO, lgl_compare_scalar); break;
  case vctrs_type_integer:   COMPARE_COL(int, INTEGER_RO, int_compare_scalar); break;
  case vctrs_type_double:    COMPARE_COL(double, REAL_RO, dbl_compare_scalar); break;
  case vctrs_type_character: COMPARE_COL(SEXP, STRING_PTR_RO, chr_compare_scalar); break;
  case vctrs_type_dataframe: df_compare_impl(p_out, p_info, x, y, na_equal); break;
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_compare()`");
  case vctrs_type_list:      Rf_errorcall(R_NilValue, "Can't compare lists with `vctrs_compare()`");
  default:                   Rf_error("Unimplemented type in `vctrs_compare()`");
  }
}

#undef COMPARE_COL
