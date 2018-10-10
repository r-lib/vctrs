#include "vctrs.h"

int equal_scalar(SEXP x, int i, SEXP y, int j, bool na_equal) {
  if (TYPEOF(x) != TYPEOF(y))
    return false;

  switch(TYPEOF(x)) {
  case LGLSXP: {
    int xi = LOGICAL(x)[i], yj = LOGICAL(y)[j];
    if (xi == NA_LOGICAL) return na_equal ? yj == NA_LOGICAL : NA_LOGICAL;
    return xi == yj;
  }
  case INTSXP: {
    int xi = INTEGER(x)[i], yj = INTEGER(y)[j];
    if (xi == NA_INTEGER) return na_equal ? yj == NA_INTEGER : NA_LOGICAL;
    return xi == yj;
  }
  case REALSXP: {
    double xi = REAL(x)[i], yj = REAL(y)[j];
    if (R_IsNA(xi)) return na_equal ? R_IsNA(yj) : NA_LOGICAL;
    if (R_IsNaN(xi)) return na_equal ? R_IsNaN(yj) : NA_LOGICAL;
    return xi == yj;
  }
  case STRSXP: {
    SEXP xi = STRING_ELT(x, i), yj = STRING_ELT(y, j);
    if (xi == NA_STRING) return na_equal ? yj == NA_STRING : NA_LOGICAL;
    // Ignoring encoding for now
    return xi == yj;
  }
  case VECSXP:
    if (is_data_frame(x)) {
      if (!is_data_frame(y))
        return false;

      int p = Rf_length(x);
      if (p != Rf_length(y))
        return false;

      if (!equal_names(x, y))
        return false;

      for (int k = 0; k < p; ++k) {
        SEXP col_x = VECTOR_ELT(x, k);
        SEXP col_y = VECTOR_ELT(y, k);
        if (!equal_scalar(col_x, i, col_y, j, na_equal))
          return false;
      }
      return true;
    } else {
      return equal_object(VECTOR_ELT(x, i), VECTOR_ELT(y, j));
    }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

bool equal_object(SEXP x, SEXP y) {
  if (x == y)
    return true;

  if (TYPEOF(x) != TYPEOF(y))
    return false;

  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case STRSXP:
  case VECSXP: {
    R_len_t n = vec_size(x);
    if (n != vec_size(y))
      return false;

    if (!equal_object(ATTRIB(x), ATTRIB(y)))
      return false;

    for (R_len_t i = 0; i < n; ++i)
      if (!equal_scalar(x, i, y, i, true))
        return false;

    return true;
  }

  case DOTSXP:
  case LANGSXP:
  case LISTSXP:
  case BCODESXP:
    if (!equal_object(ATTRIB(x), ATTRIB(y)))
      return false;
    if (!equal_object(CAR(x), CAR(y)))
      return false;
    if (!equal_object(CDR(x), CDR(y)))
      return false;
    return true;
  case CLOSXP:
    if (!equal_object(ATTRIB(x), ATTRIB(y)))
      return false;
    if (!equal_object(BODY(x), BODY(y)))
      return false;
    if (!equal_object(CLOENV(x), CLOENV(y)))
      return false;
    if (!equal_object(FORMALS(x), FORMALS(y)))
      return false;
    return true;

  // Pointer comparison
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP:
    // If equal, would have returned true above
    return false;
    break;

  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }


  return true;
}

bool equal_na(SEXP x, int i) {
  switch(TYPEOF(x)) {
  case LGLSXP:
    return LOGICAL(x)[i] == NA_LOGICAL;
  case INTSXP:
    return INTEGER(x)[i] == NA_INTEGER;
  case REALSXP:
    // is.na(NaN) is TRUE
    return isnan(REAL(x)[i]);
  case STRSXP:
    return STRING_ELT(x, i) == NA_STRING;
  case VECSXP:
    if (is_data_frame(x)) {
      int p = Rf_length(x);

      for (int k = 0; k < p; ++k) {
        SEXP col = VECTOR_ELT(x, k);
        if (equal_na(col, i))
          return true;
      }
      return false;
    } else {
      return Rf_isNull(VECTOR_ELT(x, i));
    }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

bool equal_names(SEXP x, SEXP y) {
  SEXP x_names = Rf_getAttrib(x, R_NamesSymbol);
  SEXP y_names = Rf_getAttrib(y, R_NamesSymbol);

  return equal_object(x_names, y_names);
}

// R interface -----------------------------------------------------------------

SEXP vctrs_equal(SEXP x, SEXP y, SEXP na_equal_) {
  if (TYPEOF(x) != TYPEOF(y) || vec_size(x) != vec_size(y))
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i) {
    p_out[i] = equal_scalar(x, i, y, i, na_equal);
  }

  UNPROTECT(1);
  return out;
}

SEXP vctrs_equal_na(SEXP x) {
  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i) {
    p_out[i] = equal_na(x, i);
  }

  UNPROTECT(1);
  return out;
}

SEXP vctrs_equal_object(SEXP x, SEXP y) {
  return Rf_ScalarLogical(equal_object(x, y));
}
