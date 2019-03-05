#include "vctrs.h"

int equal_scalar(SEXP x, int i, SEXP y, int j, bool na_equal) {
  if (TYPEOF(x) != TYPEOF(y))
    return false;

  switch(TYPEOF(x)) {
  case LGLSXP: {
    int xi = LOGICAL(x)[i], yj = LOGICAL(y)[j];
    if (xi == NA_LOGICAL) return na_equal ? yj == NA_LOGICAL : NA_LOGICAL;
    if (yj == NA_LOGICAL) return na_equal ? xi == NA_LOGICAL : NA_LOGICAL;
    return xi == yj;
  }
  case INTSXP: {
    int xi = INTEGER(x)[i], yj = INTEGER(y)[j];
    if (xi == NA_INTEGER) return na_equal ? yj == NA_INTEGER : NA_LOGICAL;
    if (yj == NA_INTEGER) return na_equal ? xi == NA_INTEGER : NA_LOGICAL;
    return xi == yj;
  }
  case REALSXP: {
    double xi = REAL(x)[i], yj = REAL(y)[j];
    if (R_IsNA(xi)) return na_equal ? R_IsNA(yj) : NA_LOGICAL;
    if (R_IsNaN(xi)) return na_equal ? R_IsNaN(yj) : NA_LOGICAL;
    if (R_IsNA(yj)) return na_equal ? R_IsNA(xi) : NA_LOGICAL;
    if (R_IsNaN(yj)) return na_equal ? R_IsNaN(xi) : NA_LOGICAL;
    return xi == yj;
  }
  case STRSXP: {
    SEXP xi = STRING_ELT(x, i), yj = STRING_ELT(y, j);
    if (xi == NA_STRING) return na_equal ? yj == NA_STRING : NA_LOGICAL;
    if (yj == NA_STRING) return na_equal ? xi == NA_STRING : NA_LOGICAL;
    // Ignoring encoding for now
    return xi == yj;
  }
  case VECSXP:
    if (is_data_frame(x)) {
      if (!is_data_frame(y)) {
        return false;
      }

      int p = Rf_length(x);
      if (p != Rf_length(y)) {
        return false;
      }

      // Don't worry about names missingness because properly formed
      // data frames shouldn't have any missing names
      if (!equal_names(x, y)) {
        return false;
      }

      for (int k = 0; k < p; ++k) {
        SEXP col_x = VECTOR_ELT(x, k);
        SEXP col_y = VECTOR_ELT(y, k);

        int eq = equal_scalar(col_x, i, col_y, j, na_equal);
        if (eq <= 0) {
          return eq;
        }
      }

      return true;
    } else {
      return equal_object(VECTOR_ELT(x, i), VECTOR_ELT(y, j), na_equal);
    }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// TODO: Sort attributes by tag before comparison

// We don't propagate missingness from attributes because any missing
// values in there are probably actual data
static inline bool obj_equal_attrib(SEXP x, SEXP y) {
  return equal_object(ATTRIB(x), ATTRIB(y), true);
}

// Same as `obj_` variant but propagates NA only for names
static inline int vec_equal_attrib(SEXP x, SEXP y, bool na_equal) {
  SEXP x_attrs = ATTRIB(x);
  SEXP y_attrs = ATTRIB(y);

  while (x_attrs != R_NilValue) {
    if (y_attrs == R_NilValue) {
      return false;
    }

    SEXP x_tag = TAG(x_attrs);
    SEXP y_tag = TAG(x_attrs);

    if (x_tag != y_tag) {
      return false;
    }

    int eq;
    if (x_tag == R_NamesSymbol) {
      eq = equal_object(CAR(x_attrs), CAR(y_attrs), na_equal);
    } else {
      eq = equal_object(CAR(x_attrs), CAR(y_attrs), true);
    }
    if (eq <= 0) {
      return(eq);
    }

    x_attrs = CDR(x_attrs);
    y_attrs = CDR(y_attrs);
  }

  return true;
}

int equal_object(SEXP x, SEXP y, bool na_equal) {
  SEXPTYPE type = TYPEOF(x);

  if (type != TYPEOF(y)) {
    return false;
  }

  // Pointer comparison is safe for these types
  switch (type) {
  case NILSXP:
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP:
    return x == y;
  }

  // For other types, pointer comparison is only relevant when missing
  // values are not propagated
  if (na_equal && x == y) {
    return true;
  }

  switch(type) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case STRSXP:
  case VECSXP: {
    R_len_t n = vec_size(x);
    if (n != vec_size(y)) {
      return false;
    }

    int eq_attr = vec_equal_attrib(x, y, na_equal);
    if (eq_attr <= 0) {
      return eq_attr;
    }

    for (R_len_t i = 0; i < n; ++i) {
      int eq = equal_scalar(x, i, y, i, na_equal);
      if (eq <= 0) {
        return eq;
      }
    }

    return true;
  }

  case DOTSXP:
  case LANGSXP:
  case LISTSXP:
  case BCODESXP: {
    if (!obj_equal_attrib(x, y)) {
      return false;
    }

    int eq;
    eq = equal_object(CAR(x), CAR(y), na_equal);
    if (eq <= 0) {
      return eq;
    }
    eq = equal_object(CDR(x), CDR(y), na_equal);
    if (eq <= 0) {
      return eq;
    }
    return true;
  }

  case CLOSXP:
    if (!obj_equal_attrib(x, y)) {
      return false;
    }
    if (!equal_object(BODY(x), BODY(y), true)) {
      return false;
    }
    if (!equal_object(CLOENV(x), CLOENV(y), true)) {
      return false;
    }
    if (!equal_object(FORMALS(x), FORMALS(y), true)) {
      return false;
    }
    return true;

  case NILSXP:
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP:
    // These are handled above with pointer comparison
    Rf_error("Internal error: Unexpected reference type in `vec_equal()`");

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
        if (!equal_na(col, i))
          return false;
      }
      return true;
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

  return equal_object(x_names, y_names, true);
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

SEXP vctrs_equal_object(SEXP x, SEXP y, SEXP na_equal) {
  return Rf_ScalarLogical(equal_object(x, y, Rf_asLogical(na_equal)));
}
