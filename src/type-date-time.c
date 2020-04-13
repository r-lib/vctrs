#include "vctrs.h"
#include "utils.h"


static SEXP new_date(SEXP x);
static SEXP new_datetime(SEXP x, SEXP tzone);
static SEXP new_empty_datetime(SEXP tzone);

static SEXP date_validate(SEXP x);
static SEXP datetime_validate(SEXP x);

static SEXP datetime_rezone(SEXP x, SEXP tzone);

static SEXP tzone_get(SEXP x);
static SEXP tzone_union(SEXP x_tzone, SEXP y_tzone);
static bool tzone_equal(SEXP x_tzone, SEXP y_tzone);

// -----------------------------------------------------------------------------
// ptype2

// [[ include("vctrs.h") ]]
SEXP date_datetime_ptype2(SEXP x, SEXP y) {
  SEXP x_class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  SEXP x_first_class = STRING_ELT(x_class, 0);

  SEXP tzone = (x_first_class == strings_date) ? tzone_get(y) : tzone_get(x);
  PROTECT(tzone);

  SEXP out = new_empty_datetime(tzone);

  UNPROTECT(2);
  return out;
}


// [[ include("vctrs.h") ]]
SEXP datetime_datetime_ptype2(SEXP x, SEXP y) {
  SEXP x_tzone = PROTECT(tzone_get(x));
  SEXP y_tzone = PROTECT(tzone_get(y));

  // Never allocates
  SEXP tzone = tzone_union(x_tzone, y_tzone);

  SEXP out = new_empty_datetime(tzone);

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------
// cast

// [[ include("vctrs.h") ]]
SEXP date_as_date(SEXP x) {
  return date_validate(x);
}


// [[ include("vctrs.h") ]]
SEXP date_as_posixct(SEXP x, SEXP to) {
  SEXP tzone = PROTECT(tzone_get(to));

  // Date -> character -> POSIXct
  // This is the only way to retain the same clock time
  SEXP out = PROTECT(r_as_character(x));
  out = PROTECT(r_as_posixct(out, tzone));

  UNPROTECT(3);
  return out;
}


// [[ include("vctrs.h") ]]
SEXP date_as_posixlt(SEXP x, SEXP to) {
  SEXP tzone = PROTECT(tzone_get(to));

  // Date -> character -> POSIXlt
  // This is the only way to retain the same clock time
  SEXP out = PROTECT(r_as_character(x));
  out = PROTECT(r_as_posixlt(out, tzone));

  UNPROTECT(3);
  return out;
}


// [[ include("vctrs.h") ]]
SEXP posixct_as_date(SEXP x, bool* lossy) {
  SEXP out = PROTECT(r_as_date(x));

  x = PROTECT(datetime_validate(x));
  const double* p_x = REAL(x);

  SEXP roundtrip = PROTECT(date_as_posixct(out, x));
  const double* p_roundtrip = REAL(roundtrip);

  const R_len_t size = Rf_length(out);

  for (R_len_t i = 0; i < size; ++i) {
    const double x_elt = p_x[i];

    // `NaN` and `NA` always convert without issue
    if (isnan(x_elt)) {
      continue;
    }

    const double roundtrip_elt = p_roundtrip[i];

    if (x_elt != roundtrip_elt) {
      *lossy = true;
      UNPROTECT(3);
      return R_NilValue;
    }
  }

  UNPROTECT(3);
  return out;
}


// [[ include("vctrs.h") ]]
SEXP posixlt_as_date(SEXP x, bool* lossy) {
  SEXP tzone = PROTECT(tzone_get(x));
  x = PROTECT(r_as_posixct(x, tzone));

  SEXP out = posixct_as_date(x, lossy);

  UNPROTECT(2);
  return out;
}


static SEXP posixct_as_posixct_impl(SEXP x, SEXP tzone);

// [[ include("vctrs.h") ]]
SEXP posixct_as_posixct(SEXP x, SEXP to) {
  SEXP tzone = PROTECT(tzone_get(to));
  SEXP out = posixct_as_posixct_impl(x, tzone);
  UNPROTECT(1);
  return out;
}

static SEXP posixct_as_posixct_impl(SEXP x, SEXP tzone) {
  x = PROTECT(datetime_validate(x));
  SEXP out = datetime_rezone(x, tzone);
  UNPROTECT(1);
  return out;
}


static SEXP posixlt_as_posixct_impl(SEXP x, SEXP tzone);

// [[ include("vctrs.h") ]]
SEXP posixlt_as_posixct(SEXP x, SEXP to) {
  SEXP tzone = PROTECT(tzone_get(to));
  SEXP out = posixlt_as_posixct_impl(x, tzone);
  UNPROTECT(1);
  return out;
}

static SEXP posixlt_as_posixct_impl(SEXP x, SEXP tzone) {
  SEXP x_tzone = PROTECT(tzone_get(x));
  x = PROTECT(r_as_posixct(x, x_tzone));

  SEXP out = posixct_as_posixct_impl(x, tzone);

  UNPROTECT(2);
  return out;
}


static SEXP posixct_as_posixlt_impl(SEXP x, SEXP tzone);

// [[ include("vctrs.h") ]]
SEXP posixct_as_posixlt(SEXP x, SEXP to) {
  SEXP tzone = PROTECT(tzone_get(to));
  SEXP out = posixct_as_posixlt_impl(x, tzone);
  UNPROTECT(1);
  return out;
}

static SEXP posixct_as_posixlt_impl(SEXP x, SEXP tzone) {
  return r_as_posixlt(x, tzone);
}


// [[ include("vctrs.h") ]]
SEXP posixlt_as_posixlt(SEXP x, SEXP to) {
  SEXP x_tzone = PROTECT(tzone_get(x));
  SEXP to_tzone = PROTECT(tzone_get(to));

  if (tzone_equal(x_tzone, to_tzone)) {
    UNPROTECT(2);
    return x;
  }

  SEXP out = x;

  // `as.POSIXlt.default()` doesn't respect `tz` so we have to do:
  // POSIXlt<x-tzone> -> POSIXct<x-tzone> -> POSIXct<to-tzone> -> POSIXlt<to-tzone>
  out = PROTECT(posixlt_as_posixct_impl(out, x_tzone));
  out = PROTECT(posixct_as_posixct_impl(out, to_tzone));
  out = PROTECT(posixct_as_posixlt_impl(out, to_tzone));

  UNPROTECT(5);
  return out;
}

// -----------------------------------------------------------------------------

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


static SEXP new_empty_datetime(SEXP tzone) {
  return new_datetime(vctrs_shared_empty_dbl, tzone);
}

// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_datetime_rezone(SEXP x, SEXP tzone) {
  return datetime_rezone(x, tzone);
}

// Same underlying numeric representation, different `tzone`
static SEXP datetime_rezone(SEXP x, SEXP tzone) {
  SEXP x_tzone = PROTECT(tzone_get(x));

  if (tzone_equal(x_tzone, tzone)) {
    UNPROTECT(1);
    return x;
  }

  SEXP out = PROTECT(r_maybe_duplicate(x));

  Rf_setAttrib(out, syms_tzone, tzone);

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------
// Time zone utilities

static SEXP tzone_get(SEXP x) {
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

// `tzone_get()` is guaranteed to return 1 element
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

// `tzone_get()` is guaranteed to return 1 element
static bool tzone_equal(SEXP x_tzone, SEXP y_tzone) {
  // Equal objects?
  if (x_tzone == y_tzone) {
    return true;
  }

  // Equal CHARSXPs?
  SEXP x_string = STRING_ELT(x_tzone, 0);
  SEXP y_string = STRING_ELT(y_tzone, 0);

  if (x_string == y_string) {
    return true;
  }

  // Equal C char?
  const char* x_tzone_char = CHAR(x_string);
  const char* y_tzone_char = CHAR(y_string);

  return !strcmp(x_tzone_char, y_tzone_char);
}
