#include "vctrs.h"

// Initialised at load time
static SEXP vec_cast_dispatch_fn = NULL;


static bool is_lossy_int_as_lgl(SEXP x) {
  int* data = INTEGER(x);
  R_len_t n = Rf_length(x);

  for (R_len_t i = 0; i < n; ++i, ++data) {
    int elt = *data;
    if (elt != 0 && elt != 1) {
      return true;
    }
  }

  return false;
}
static bool is_lossy_dbl_as_lgl(SEXP x) {
  double* data = REAL(x);
  R_len_t n = Rf_length(x);

  for (R_len_t i = 0; i < n; ++i, ++data) {
    int elt = *data;
    if (elt != 0 && elt != 1) {
      return true;
    }
  }

  return false;
}
static bool is_lossy_chr_as_lgl(SEXP x) {
  SEXP* data = STRING_PTR(x);
  R_len_t n = Rf_length(x);

  for (R_len_t i = 0; i < n; ++i, ++data) {
    const char* elt = CHAR(*data);
    switch (elt[0]) {
    case 'T':
      if (elt[1] == '\0' || strcmp(elt, "TRUE") == 0) {
        continue;
      } else {
        return true;
      }
    case 'F':
      if (elt[1] == '\0' || strcmp(elt, "TRUE") == 0) {
        continue;
      } else {
        return true;
      }
    case 't':
      if (strcmp(elt, "true") == 0) {
        continue;
      } else {
        return true;
      }
    case 'f':
      if (strcmp(elt, "false") == 0) {
        continue;
      } else {
        return true;
      }
    default:
      return true;
    }
  }

  return false;
}

static SEXP dbl_as_integer(SEXP x, bool* lossy) {
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

SEXP vec_cast(SEXP x, SEXP to) {
  if (x == R_NilValue || to == R_NilValue) {
    return x;
  }
  if (has_dim(x) || has_dim(to)) {
    goto dispatch;
  }

  switch (vec_typeof(to)) {
  case vctrs_type_logical:
    switch (vec_typeof(x)) {
    case vctrs_type_logical:
      return x;
    case vctrs_type_integer:
      if (is_lossy_int_as_lgl(x)) goto dispatch; else return Rf_coerceVector(x, LGLSXP);
    case vctrs_type_double:
      if (is_lossy_dbl_as_lgl(x)) goto dispatch; else return Rf_coerceVector(x, LGLSXP);
    case vctrs_type_character:
      if (is_lossy_chr_as_lgl(x)) goto dispatch; else return Rf_coerceVector(x, LGLSXP);
    // TODO case vctrs_type_list:
    default:
      goto dispatch;
    }

  case vctrs_type_integer:
    switch (vec_typeof(x)) {
    case vctrs_type_logical:
      return Rf_coerceVector(x, INTSXP);
    case vctrs_type_integer:
      return x;
    case vctrs_type_double: {
      bool lossy = false;
      SEXP out = dbl_as_integer(x, &lossy);
      if (lossy) goto dispatch; else return out;
    }
    default:
      goto dispatch;
    }

  default:
  dispatch: {
    SEXP dispatch_call = PROTECT(Rf_lang3(vec_cast_dispatch_fn, x, to));
    SEXP out = Rf_eval(dispatch_call, R_GlobalEnv);

    UNPROTECT(1);
    return out;
  }}
}


void vctrs_init_cast(SEXP ns) {
  vec_cast_dispatch_fn = Rf_findVar(Rf_install("vec_cast_dispatch"), ns);
}
