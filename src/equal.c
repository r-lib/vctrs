#include "vctrs.h"

bool equal_scalar(SEXP x, int i, SEXP y, int j) {
  if (TYPEOF(x) != TYPEOF(y))
    return false;

  switch(TYPEOF(x)) {
  case LGLSXP:
    return LOGICAL(x)[i] == LOGICAL(y)[j];
  case INTSXP:
    return INTEGER(x)[i] == INTEGER(y)[j];
  case REALSXP: {
    double xi = REAL(x)[i], yj = REAL(y)[j];
    if (R_IsNA(xi)) return R_IsNA(yj);
    if (R_IsNaN(xi)) return R_IsNaN(yj);
    return xi == yj;
  }
  case STRSXP:
    // Ignoring encoding for now
    return STRING_ELT(x, i) == STRING_ELT(y, j);
  case VECSXP:
    if (is_data_frame(x)) {
      int p = Rf_length(x);
      if (p != Rf_length(y))
        return false;
      if (!equal_object(Rf_getAttrib(x, R_NamesSymbol), Rf_getAttrib(y, R_NamesSymbol)))
        return false;

      for (int k = 0; k < p; ++k) {
        SEXP col_x = VECTOR_ELT(x, k);
        SEXP col_y = VECTOR_ELT(y, k);
        if (!equal_scalar(col_x, i, col_y, j))
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
  case NILSXP:
    return true;

  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case STRSXP:
  case VECSXP: {
    R_len_t n = vec_length(x);
    if (n != vec_length(y))
      return false;

    if (!equal_object(ATTRIB(x), ATTRIB(y)))
      return false;

    for (R_len_t i = 0; i < n; ++i)
      if (!equal_scalar(x, i, y, i))
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
    return x == y;
    break;

  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }


  return true;
}

// R interface -----------------------------------------------------------------

SEXP vctrs_equal(SEXP x, SEXP y) {
  if (TYPEOF(x) != TYPEOF(y) || vec_length(x) != vec_length(y))
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");

  R_len_t n = vec_length(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i) {
    p_out[i] = equal_scalar(x, i, y, i);
  }

  UNPROTECT(1);
  return out;
}

SEXP vctrs_equal_object(SEXP x, SEXP y) {
  return Rf_ScalarLogical(equal_object(x, y));
}
