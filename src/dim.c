#include "vctrs.h"
#include "utils.h"

R_len_t rcrd_size(SEXP x);

R_len_t vec_size(SEXP x) {
  switch(TYPEOF(x)) {
  case NILSXP:
    return 0;

  case VECSXP:
    if (is_scalar(x)) {
      Rf_errorcall(R_NilValue, "`x` is a scalar");
    } else if (is_data_frame(x)) {
      return df_size(x);
    } else if (is_record(x)) {
      return rcrd_size(x);
    }
    // Fall through to non-list logic

  case EXPRSXP:
  case LGLSXP:
  case INTSXP:
  case RAWSXP:
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

R_len_t df_rownames_size(SEXP x) {
  for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr)) {
    if (TAG(attr) != R_RowNamesSymbol) {
      continue;
    }

    SEXP rn = CAR(attr);
    R_len_t n = Rf_length(rn);

    switch(TYPEOF(rn)) {
    case INTSXP:
      if (is_compact_rownames(rn)) {
        return compact_rownames_length(rn);
      } else {
        return n;
      }
    case STRSXP:
      return n;
    default:
      Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are invalid type");
    }
  }

  return -1;
}

// For performance, avoid Rf_getAttrib() because it automatically transforms
// the rownames into an integer vector
R_len_t df_size(SEXP x) {
  R_len_t n = df_rownames_size(x);

  if (n < 0) {
    Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are missing");
  }

  return n;
}
// Supports bare lists as well
R_len_t df_raw_size(SEXP x) {
  R_len_t n = df_rownames_size(x);
  if (n >= 0) {
    return n;
  }

  if (Rf_length(x) >= 1) {
    return vec_size(VECTOR_ELT(x, 0));
  } else {
    return 0;
  }
}


R_len_t rcrd_size(SEXP x) {
  int n = Rf_length(x);
  if (n == 0) {
    return 0;
  } else {
    return Rf_length(VECTOR_ELT(x, 0));
  }
}

bool has_dim(SEXP x) {
  return ATTRIB(x) != R_NilValue && Rf_getAttrib(x, R_DimSymbol) != R_NilValue;
}

// R interface ------------------------------------------------------------

SEXP vctrs_size(SEXP x) {
  return Rf_ScalarInteger(vec_size(x));
}
