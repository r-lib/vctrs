#include <math.h>
#include "vctrs.h"

static int lgl_equal_scalar(const int* x, const int* y, bool na_equal);
static int int_equal_scalar(const int* x, const int* y, bool na_equal);
static int dbl_equal_scalar(const double* x, const double* y, bool na_equal);
static int chr_equal_scalar(const SEXP* x, const SEXP* y, bool na_equal);
static int list_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);
static int df_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);


// [[ include("vctrs.h") ]]
int equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_equal_scalar(LOGICAL(x) + i, LOGICAL(y) + j, na_equal);
  case INTSXP: return int_equal_scalar(INTEGER(x) + i, INTEGER(y) + j, na_equal);
  case REALSXP: return dbl_equal_scalar(REAL(x) + i, REAL(y) + j, na_equal);
  case STRSXP: return chr_equal_scalar(STRING_PTR(x) + i, STRING_PTR(y) + j, na_equal);
  default: break;
  }

  switch (vec_proxy_typeof(x)) {
  case vctrs_type_list: return list_equal_scalar(x, i, y, j, na_equal);
  case vctrs_type_dataframe: return df_equal_scalar(x, i, y, j, na_equal);
  default: break;
  }

  vctrs_stop_unsupported_type(vec_typeof(x), "equal_scalar()");
}


#define EQUAL(CTYPE, CONST_DEREF, SCALAR_EQUAL)         \
  do {                                                  \
    const CTYPE* xp = CONST_DEREF(x);                   \
    const CTYPE* yp = CONST_DEREF(y);                   \
                                                        \
    for (R_len_t i = 0; i < n; ++i, ++xp, ++yp) {       \
      p[i] = SCALAR_EQUAL(xp, yp, na_equal);            \
    }                                                   \
  }                                                     \
  while (0)

#define EQUAL_BARRIER(SCALAR_EQUAL)                     \
  do {                                                  \
    for (R_len_t i = 0; i < n; ++i) {                   \
      p[i] = SCALAR_EQUAL(x, i, y, i, na_equal);        \
    }                                                   \
  }                                                     \
  while (0)

// [[ register() ]]
SEXP vctrs_equal(SEXP x, SEXP y, SEXP na_equal_) {
  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || vec_size(x) != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p = LOGICAL(out);

  switch (type) {
  case vctrs_type_logical:   EQUAL(int, LOGICAL_RO, lgl_equal_scalar); break;
  case vctrs_type_integer:   EQUAL(int, INTEGER_RO, int_equal_scalar); break;
  case vctrs_type_double:    EQUAL(double, REAL_RO, dbl_equal_scalar); break;
  case vctrs_type_character: EQUAL(SEXP, STRING_PTR_RO, chr_equal_scalar); break;
  case vctrs_type_list:      EQUAL_BARRIER(list_equal_scalar); break;
  case vctrs_type_dataframe: EQUAL_BARRIER(df_equal_scalar); break;
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_equal()`");
  default:                   Rf_error("Unimplemented type in `vctrs_equal()`");
  }

  UNPROTECT(1);
  return out;
}

#undef EQUAL
#undef EQUAL_BARRIER

// Storing pointed values on the stack helps performance for the
// `!na_equal` cases
static int lgl_equal_scalar(const int* x, const int* y, bool na_equal) {
  const int xi = *x;
  const int yj = *y;
  if (na_equal) {
    return xi == yj;
  } else {
    return (xi == NA_LOGICAL || yj == NA_LOGICAL) ? NA_LOGICAL : xi == yj;
  }
}
static int int_equal_scalar(const int* x, const int* y, bool na_equal) {
  const int xi = *x;
  const int yj = *y;
  if (na_equal) {
    return xi == yj;
  } else {
    return (xi == NA_INTEGER || yj == NA_INTEGER) ? NA_LOGICAL : xi == yj;
  }
}
static int dbl_equal_scalar(const double* x, const double* y, bool na_equal) {
  const double xi = *x;
  const double yj = *y;
  if (na_equal) {
    if (R_IsNA(xi)) return R_IsNA(yj);
    if (R_IsNaN(xi)) return R_IsNaN(yj);
    if (R_IsNA(yj)) return false;
    if (R_IsNaN(yj)) return false;
  } else {
    if (isnan(xi) || isnan(yj)) return NA_LOGICAL;
  }
  return xi == yj;
}
static int chr_equal_scalar(const SEXP* x, const SEXP* y, bool na_equal) {
  const SEXP xi = *x;
  const SEXP yj = *y;
  if (na_equal) {
    // Ignoring encoding for now
    return xi == yj;
  } else {
    return (xi == NA_STRING || yj == NA_STRING) ? NA_LOGICAL : xi == yj;
  }
}

static int list_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  return equal_object(VECTOR_ELT(x, i), VECTOR_ELT(y, j), na_equal);
}

static int df_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
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
    SEXP col_x = PROTECT(vec_proxy_equal(VECTOR_ELT(x, k)));
    SEXP col_y = PROTECT(vec_proxy_equal(VECTOR_ELT(y, k)));

    int eq = equal_scalar(col_x, i, col_y, j, na_equal);
    UNPROTECT(2);

    if (eq <= 0) {
      return eq;
    }
  }

  return true;
}


static inline bool obj_equal_attrib(SEXP x, SEXP y);
static inline int vec_equal_attrib(SEXP x, SEXP y, bool na_equal);

// [[ include("vctrs.h") ]]
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

// [[ register() ]]
SEXP vctrs_equal_object(SEXP x, SEXP y, SEXP na_equal) {
  return Rf_ScalarLogical(equal_object(x, y, Rf_asLogical(na_equal)));
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


// [[ include("vctrs.h") ]]
bool equal_names(SEXP x, SEXP y) {
  SEXP x_names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  SEXP y_names = PROTECT(Rf_getAttrib(y, R_NamesSymbol));

  bool out = equal_object(x_names, y_names, true);

  UNPROTECT(2);
  return out;
}


static bool equal_na(SEXP x, int i) {
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

// [[ register() ]]
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
