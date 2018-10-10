#include "vctrs.h"

R_len_t df_obs(SEXP x);
R_len_t rcrd_obs(SEXP x);

R_len_t vec_size(SEXP x) {
  switch(TYPEOF(x)) {
  case NILSXP:
    return 0;

  case VECSXP:
    if (is_scalar(x)) {
      Rf_errorcall(R_NilValue, "`x` is a scalar");
    } else if (is_data_frame(x)) {
      return df_obs(x);
    } else if (is_record(x)) {
      return rcrd_obs(x);
    }
    // Fall through to non-list logic

  case EXPRSXP:
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP: {
    SEXP dims = Rf_getAttrib(x, R_DimSymbol);
    if (dims == R_NilValue || Rf_length(dims) == 0)
      return Rf_length(x);

    if (TYPEOF(dims) != INTSXP)
      Rf_errorcall(R_NilValue, "Corrupt vector: dims is not integer vector");

    return INTEGER(dims)[0];
  }

  default:
    Rf_errorcall(R_NilValue, "`x` is a not a vector");
  }
}

// For performance, avoid Rf_getAttrib() because it automatically transforms
// the rownames into an integer vector
R_len_t df_obs(SEXP x) {
  for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr)) {
    if (TAG(attr) != R_RowNamesSymbol)
      continue;

    SEXP rn = CAR(attr);
    R_len_t n = Rf_length(rn);

    switch(TYPEOF(rn)) {
    case INTSXP:
      if (n == 2 && INTEGER(rn)[0] == NA_INTEGER) {
        return -INTEGER(rn)[1];
      } else {
        return n;
      }
    case STRSXP:
      return n;
    default:
      Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are invalid type");
    }
  }
  Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are missing");
}

R_len_t rcrd_obs(SEXP x) {
  int n = Rf_length(x);
  if (n == 0) {
    return 0;
  } else {
    return Rf_length(VECTOR_ELT(x, 0));
  }
}

// R interface ------------------------------------------------------------

SEXP vctrs_size(SEXP x) {
  return Rf_ScalarInteger(vec_size(x));
}
