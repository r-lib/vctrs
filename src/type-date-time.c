#include "vctrs.h"
#include "utils.h"

static SEXP new_empty_datetime(SEXP tzone);
static SEXP get_tzone(SEXP x);

// [[ include("vctrs.h") ]]
SEXP date_datetime_ptype2(SEXP x, SEXP y) {
  SEXP x_class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  SEXP x_first_class = STRING_ELT(x_class, 0);

  SEXP tzone = (x_first_class == strings_date) ? get_tzone(y) : get_tzone(x);
  PROTECT(tzone);

  SEXP out = new_empty_datetime(tzone);

  UNPROTECT(2);
  return out;
}

static SEXP tzone_union(SEXP x_tzone, SEXP y_tzone);

// [[ include("vctrs.h") ]]
SEXP datetime_datetime_ptype2(SEXP x, SEXP y) {
  SEXP x_tzone = PROTECT(get_tzone(x));
  SEXP y_tzone = PROTECT(get_tzone(y));

  // Never allocates
  SEXP tzone = tzone_union(x_tzone, y_tzone);

  SEXP out = new_empty_datetime(tzone);

  UNPROTECT(2);
  return out;
}


static SEXP new_empty_datetime(SEXP tzone) {
  SEXP out = PROTECT(Rf_allocVector(REALSXP, 0));

  Rf_setAttrib(out, R_ClassSymbol, classes_posixct);
  Rf_setAttrib(out, syms_tzone, tzone);

  UNPROTECT(1);
  return out;
}

static SEXP get_tzone(SEXP x) {
  SEXP tzone = PROTECT(Rf_getAttrib(x, syms_tzone));

  if (tzone == R_NilValue) {
    UNPROTECT(1);
    return chrs_empty;
  }

  R_len_t size = Rf_length(tzone);

  if (size == 1) {
    UNPROTECT(1);
    return tzone;
  }

  if (size == 0) {
    Rf_errorcall(R_NilValue, "Corrupt datetime with 0-length `tzone` attribute");
  }

  // If there are multiple, only take the first
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, STRING_ELT(tzone, 0));

  UNPROTECT(2);
  return out;
}

// `get_tzone()` is guaranteed to return 1 element
static inline bool tzone_is_local(SEXP tzone) {
  return STRING_ELT(tzone, 0) == strings_empty;
}

static SEXP tzone_union(SEXP x_tzone, SEXP y_tzone) {
  if (tzone_is_local(x_tzone)) {
    return y_tzone;
  } else {
    return x_tzone;
  }
}
