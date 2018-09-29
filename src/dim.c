#include "vctrs.h"

bool is_data_frame(SEXP x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "data.frame");
}

bool is_record(SEXP x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "vctrs_rcrd");
}

bool is_scalar(SEXP x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "vctrs_sclr");
}

// For performance, avoid Rf_getAttrib() because it automatically transforms
// the rownames into an integer vector
R_len_t df_rownames(SEXP x) {
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

R_len_t vec_obs(SEXP x) {
  if (!Rf_isVector(x) && !Rf_isNull(x))
    Rf_errorcall(R_NilValue, "`x` is a not a vector");

  if (is_scalar(x)) {
    Rf_errorcall(R_NilValue, "`x` is a scalar");
  }
  if (is_data_frame(x)) {
    return df_rownames(x);
  }
  if (is_record(x)) {
    int n = Rf_length(x);
    if (n == 0) {
      return 0;
    } else {
      return Rf_length(VECTOR_ELT(x, 0));
    }
  }

  SEXP dims = Rf_getAttrib(x, R_DimSymbol);
  if (dims == R_NilValue || Rf_length(dims) == 0)
    return Rf_length(x);

  if (TYPEOF(dims) != INTSXP)
    Rf_errorcall(R_NilValue, "Corrupt vector: dims is not integer vector");

  return INTEGER(dims)[0];
}

// R interface ------------------------------------------------------------

SEXP vctrs_length(SEXP x) {
  return Rf_ScalarInteger(vec_obs(x));
}
