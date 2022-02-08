#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

// [[ include("cast.h") ]]
SEXP int_as_logical(SEXP x, bool* lossy) {
  int* data = INTEGER(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* out_data = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;

    if (elt == NA_INTEGER) {
      *out_data = NA_LOGICAL;
      continue;
    }

    if (elt != 0 && elt != 1) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    *out_data = elt;
  }

  UNPROTECT(1);
  return out;
}

// [[ include("cast.h") ]]
SEXP dbl_as_logical(SEXP x, bool* lossy) {
  double* data = REAL(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* out_data = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    double elt = *data;

    if (isnan(elt)) {
      *out_data = NA_LOGICAL;
      continue;
    }

    if (elt != 0 && elt != 1) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    *out_data = (int) elt;
  }

  UNPROTECT(1);
  return out;
}

// [[ include("cast.h") ]]
SEXP chr_as_logical(SEXP x, bool* lossy) {
  SEXP const* x_p = STRING_PTR_RO(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_out = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP str = x_p[i];
    if (str == NA_STRING) {
      p_out[i] = NA_LOGICAL;
      continue;
    }

    const char* elt = CHAR(str);
    switch (elt[0]) {
    case 'T':
      if (elt[1] == '\0' || strcmp(elt, "TRUE") == 0) {
        p_out[i] = 1;
        continue;
      }
      break;
    case 'F':
      if (elt[1] == '\0' || strcmp(elt, "FALSE") == 0) {
        p_out[i] = 0;
        continue;
      }
      break;
    case 't':
      if (strcmp(elt, "true") == 0) {
        p_out[i] = 1;
        continue;
      }
      break;
    case 'f':
      if (strcmp(elt, "false") == 0) {
        p_out[i] = 0;
        continue;
      }
      break;
    default:
      break;
    }

    *lossy = true;
    UNPROTECT(1);
    return R_NilValue;
  }

  UNPROTECT(1);
  return out;
}

// [[ include("cast.h") ]]
SEXP lgl_as_integer(SEXP x, bool* lossy) {
  return Rf_coerceVector(x, INTSXP);
}

// [[ include("cast.h") ]]
SEXP dbl_as_integer(SEXP x, bool* lossy) {
  double* data = REAL(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int* out_data = INTEGER(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    double elt = *data;

    if (elt <= INT_MIN || elt >= INT_MAX + 1.0) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    if (isnan(elt)) {
      *out_data = NA_INTEGER;
      continue;
    }

    int value = (int) elt;

    if (value != elt) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    *out_data = value;
  }

  UNPROTECT(1);
  return out;
}

// [[ include("cast.h") ]]
SEXP lgl_as_double(SEXP x, bool* lossy) {
  int* data = LOGICAL(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double* out_data = REAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;
    *out_data = (elt == NA_LOGICAL) ? NA_REAL : elt;
  }

  UNPROTECT(1);
  return out;
}

// [[ include("cast.h") ]]
SEXP int_as_double(SEXP x, bool* lossy) {
  int* data = INTEGER(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double* out_data = REAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;
    *out_data = (elt == NA_INTEGER) ? NA_REAL : elt;
  }

  UNPROTECT(1);
  return out;
}
