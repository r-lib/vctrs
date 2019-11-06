#include "vctrs.h"
#include <strings.h>

static void stop_not_comparable(SEXP x, SEXP y, const char* message) {
  Rf_errorcall(R_NilValue, "`x` and `y` are not comparable: %s", message);
}

// https://stackoverflow.com/questions/10996418
static int icmp(int x, int y) {
  return (x > y) - (x < y);
}

static int dcmp(double x, double y) {
  return (x > y) - (x < y);
}

static int scmp(SEXP x, SEXP y) {
  if (x == y)
    return 0;
  int cmp = strcmp(CHAR(x), CHAR(y));
  return cmp / abs(cmp);
}

// -----------------------------------------------------------------------------

static int lgl_compare_scalar(const int* x, const int* y, bool na_equal) {
  int xi = *x;
  int yj = *y;

  if (na_equal) {
    return icmp(xi, yj);
  } else {
    return (xi == NA_LOGICAL || yj == NA_LOGICAL) ? NA_INTEGER : icmp(xi, yj);
  }
}

static int int_compare_scalar(const int* x, const int* y, bool na_equal) {
  int xi = *x;
  int yj = *y;

  if (na_equal) {
    return icmp(xi, yj);
  } else {
    return (xi == NA_INTEGER || yj == NA_INTEGER) ? NA_INTEGER : icmp(xi, yj);
  }
}

static int dbl_compare_scalar(const double* x, const double* y, bool na_equal) {
  double xi = *x;
  double yj = *y;

  if (na_equal) {
    if (R_IsNA(xi)) {
      if (R_IsNaN(yj)) {
        return 1;
      } else if (R_IsNA(yj)) {
        return 0;
      } else {
        return -1;
      }
    } else if (R_IsNaN(xi)) {
      if (R_IsNaN(yj)) {
        return 0;
      } else if (R_IsNA(yj)) {
        return -1;
      } else {
        return -1;
      }
    } else {
      if (R_IsNaN(yj)) {
        return 1L;
      } else if (R_IsNA(yj)) {
        return 1L;
      } else {
        return dcmp(xi, yj);
      }
    }
  } else {
    return (isnan(xi) || isnan(yj)) ? NA_INTEGER : dcmp(xi, yj);
  }
}

// TODO - Handle translations
static int chr_compare_scalar(const SEXP* x, const SEXP* y, bool na_equal) {
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

static int df_compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal, int n_col) {
  if (n_col == 0) {
    stop_not_comparable(x, y, "data frame with zero columns");
  }

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
  case LGLSXP: return lgl_compare_scalar(LOGICAL(x) + i, LOGICAL(y) + j, na_equal);
  case INTSXP: return int_compare_scalar(INTEGER(x) + i, INTEGER(y) + j, na_equal);
  case REALSXP: return dbl_compare_scalar(REAL(x) + i, REAL(y) + j, na_equal);
  case STRSXP: return chr_compare_scalar(STRING_PTR(x) + i, STRING_PTR(y) + j, na_equal);
  default: break;
  }

  switch (vec_proxy_typeof(x)) {
  case vctrs_type_list: stop_not_comparable(x, y, "lists are not comparable");
  case vctrs_type_dataframe: {
    int n_col = Rf_length(x);

    if (n_col != Rf_length(y)) {
      stop_not_comparable(x, y, "must have the same number of columns");
    }

    return df_compare_scalar(x, i, y, j, na_equal, n_col);
  }
  default: break;
  }

  Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
}

// -----------------------------------------------------------------------------

#define COMPARE(CTYPE, CONST_DEREF, SCALAR_COMPARE)     \
do {                                                    \
  const CTYPE* xp = CONST_DEREF(x);                     \
  const CTYPE* yp = CONST_DEREF(y);                     \
                                                        \
  for (R_len_t i = 0; i < n; ++i, ++xp, ++yp) {         \
    p[i] = SCALAR_COMPARE(xp, yp, na_equal);            \
  }                                                     \
}                                                       \
while (0)

#define COMPARE_DF(SCALAR_COMPARE)                                     \
do {                                                                   \
  int n_col = Rf_length(x);                                            \
                                                                       \
  if (n_col != Rf_length(y)) {                                         \
    stop_not_comparable(x, y, "must have the same number of columns"); \
  }                                                                    \
                                                                       \
  for (R_len_t i = 0; i < n; ++i) {                                    \
    p[i] = SCALAR_COMPARE(x, i, y, i, na_equal, n_col);                \
  }                                                                    \
}                                                                      \
while (0)


// [[ register() ]]
SEXP vctrs_compare(SEXP x, SEXP y, SEXP na_equal_) {
  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);

  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || n != vec_size(y)) {
    stop_not_comparable(x, y, "must have the same types and lengths");
  }

  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int32_t* p = INTEGER(out);

  switch (type) {
  case vctrs_type_logical:   COMPARE(int, LOGICAL_RO, lgl_compare_scalar); break;
  case vctrs_type_integer:   COMPARE(int, INTEGER_RO, int_compare_scalar); break;
  case vctrs_type_double:    COMPARE(double, REAL_RO, dbl_compare_scalar); break;
  case vctrs_type_character: COMPARE(SEXP, STRING_PTR_RO, chr_compare_scalar); break;
  case vctrs_type_dataframe: COMPARE_DF(df_compare_scalar); break;
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_compare()`");
  case vctrs_type_list:      Rf_errorcall(R_NilValue, "Can't compare lists with `vctrs_compare()`");
  default:                   Rf_error("Unimplemented type in `vctrs_compare()`");
  }

  UNPROTECT(1);
  return out;
}

#undef COMPARE
#undef COMPARE_DF
