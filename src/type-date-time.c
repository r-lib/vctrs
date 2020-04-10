#include "vctrs.h"
#include "utils.h"

static SEXP new_date(SEXP x);

// [[ register() ]]
SEXP vctrs_new_date(SEXP x) {
  return new_date(x);
}

static SEXP new_date(SEXP x) {
  if (TYPEOF(x) != REALSXP) {
    Rf_errorcall(R_NilValue, "`x` must be a double vector.");
  }

  SEXP names = PROTECT(r_names(x));

  SEXP out = PROTECT(r_maybe_duplicate(x));

  SET_ATTRIB(out, R_NilValue);

  r_poke_names(out, names);
  r_poke_class(out, classes_date);

  UNPROTECT(2);
  return out;
}


static SEXP date_validate(SEXP x);

// [[ register() ]]
SEXP vctrs_date_validate(SEXP x) {
  return date_validate(x);
}

// Ensure that a `Date` is internally stored as a double vector
static SEXP date_validate(SEXP x) {
  switch (TYPEOF(x)) {
  case REALSXP:
    return x;
  case INTSXP:
    // Keeps attributes
    return Rf_coerceVector(x, REALSXP);
  default:
    Rf_errorcall(
      R_NilValue,
      "Internal error: Corrupt `Date` with unknown type %s.", Rf_type2char(TYPEOF(x))
    );
  }
}

// -----------------------------------------------------------------------------

static SEXP new_datetime(SEXP x, SEXP tzone);

// [[ register() ]]
SEXP vctrs_new_datetime(SEXP x, SEXP tzone) {
  return new_datetime(x, tzone);
}

static SEXP new_datetime(SEXP x, SEXP tzone) {
  if (TYPEOF(x) != REALSXP) {
    Rf_errorcall(R_NilValue, "`x` must be a double vector.");
  }

  // Convenience special case where we allow a
  // null `tzone` to represent local time
  if (tzone == R_NilValue) {
    tzone = chrs_empty;
  }

  if (TYPEOF(tzone) != STRSXP) {
    Rf_errorcall(R_NilValue, "`tzone` must be a character vector or `NULL`.");
  }

  SEXP names = PROTECT(r_names(x));

  SEXP out = PROTECT(r_maybe_duplicate(x));

  SET_ATTRIB(out, R_NilValue);

  r_poke_names(out, names);
  r_poke_class(out, classes_posixct);
  Rf_setAttrib(out, syms_tzone, tzone);

  UNPROTECT(2);
  return out;
}


static SEXP datetime_validate(SEXP x);

// [[ register() ]]
SEXP vctrs_datetime_validate(SEXP x) {
  return datetime_validate(x);
}

static SEXP datetime_validate_tzone(SEXP x);
static SEXP datetime_validate_type(SEXP x);

// Ensure that a `POSIXct` is internally stored as a double vector.
// Also checks that the `tzone` attribute is non-NULL.
static SEXP datetime_validate(SEXP x) {
  x = PROTECT(datetime_validate_tzone(x));
  x = PROTECT(datetime_validate_type(x));
  UNPROTECT(2);
  return x;
}

static SEXP datetime_validate_tzone(SEXP x) {
  SEXP tzone = Rf_getAttrib(x, syms_tzone);

  if (tzone != R_NilValue) {
    return x;
  }

  x = PROTECT(r_maybe_duplicate(x));

  Rf_setAttrib(x, syms_tzone, chrs_empty);

  UNPROTECT(1);
  return x;
}

static SEXP datetime_validate_type(SEXP x) {
  switch (TYPEOF(x)) {
  case REALSXP:
    return x;
  case INTSXP:
    // Keeps attributes
    return Rf_coerceVector(x, REALSXP);
  default:
    Rf_errorcall(
      R_NilValue,
      "Internal error: Corrupt `POSIXct` with unknown type %s.", Rf_type2char(TYPEOF(x))
    );
  }

  never_reached("datetime_validate_type");
}


static SEXP datetime_rezone(SEXP x, SEXP tzone);

// [[ register() ]]
SEXP vctrs_datetime_rezone(SEXP x, SEXP tzone) {
  return datetime_rezone(x, tzone);
}

// Same underlying numeric representation, different `tzone`
static SEXP datetime_rezone(SEXP x, SEXP tzone) {
  SEXP x_tzone = PROTECT(Rf_getAttrib(x, syms_tzone));

  if (x_tzone == tzone) {
    UNPROTECT(1);
    return x;
  }

  SEXP out = PROTECT(r_maybe_duplicate(x));

  Rf_setAttrib(out, syms_tzone, tzone);

  UNPROTECT(2);
  return out;
}


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
  return new_datetime(vctrs_shared_empty_dbl, tzone);
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
