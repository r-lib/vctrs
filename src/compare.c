#include "vctrs.h"
#include <strings.h>

void stop_not_comparable(SEXP x, SEXP y, const char* message) {
  Rf_errorcall(R_NilValue, "`x` and `y` are not comparable: %s", message);
}

// https://stackoverflow.com/questions/10996418
int icmp(int x, int y) {
  return (x > y) - (x < y);
}
int dcmp(double x, double y) {
  return (x > y) - (x < y);
}

int scmp(SEXP x, SEXP y) {
  if (x == y)
    return 0;
  int cmp = strcmp(CHAR(x), CHAR(y));
  return cmp / abs(cmp);
}

int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  if (TYPEOF(x) != TYPEOF(y))
    stop_not_comparable(x, y, "different types");

  switch(TYPEOF(x)) {
  case LGLSXP: {
    int xi = LOGICAL(x)[i], yj = LOGICAL(y)[j];
    if (na_equal) {
      return icmp(xi, yj);
    } else {
      return (xi == NA_LOGICAL || yj == NA_LOGICAL) ? NA_INTEGER : icmp(xi, yj);
    }
  }
  case INTSXP: {
    int xi = INTEGER(x)[i], yj = INTEGER(y)[j];
    if (na_equal) {
      return icmp(xi, yj);
    } else {
      return (xi == NA_INTEGER || yj == NA_INTEGER) ? NA_INTEGER : icmp(xi, yj);
    }
  }
  case REALSXP: {
    double xi = REAL(x)[i], yj = REAL(y)[j];
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
  case STRSXP: {
    SEXP xi = STRING_ELT(x, i), yj = STRING_ELT(y, j);
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
  case VECSXP:
    if (is_data_frame(x)) {
      int p = Rf_length(x);
      if (p != Rf_length(y))
        stop_not_comparable(x, y, "different number of columns");
      if (!equal_names(x, y))
        stop_not_comparable(x, y, "different column names");

      if (p == 0)
        stop_not_comparable(x, y, "data frame with zero columns");

      if (p > 1) {
        for (int k = 0; k < (p - 1); ++k) {
          SEXP col_x = VECTOR_ELT(x, k);
          SEXP col_y = VECTOR_ELT(y, k);
          int cmp = compare_scalar(col_x, i, col_y, j, na_equal);

          if (cmp != 0)
            return cmp;
        }
      }

      SEXP col_x = VECTOR_ELT(x, p - 1);
      SEXP col_y = VECTOR_ELT(y, p - 1);
      return compare_scalar(col_x, i, col_y, j, na_equal);
    } else {
      stop_not_comparable(x, y, "lists are not comparable");
    }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP vctrs_compare(SEXP x, SEXP y, SEXP na_compare_) {
  bool na_compare = Rf_asLogical(na_compare_);
  R_len_t n = vec_size(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int32_t* p_out = INTEGER(out);

  for (R_len_t i = 0; i < n; ++i) {
    p_out[i] = compare_scalar(x, i, y, i, na_compare);
  }

  UNPROTECT(1);
  return out;
}
