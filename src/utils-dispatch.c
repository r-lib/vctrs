#include "vctrs.h"
#include "utils.h"


// Defined below
enum vctrs_class_type class_type(SEXP x);
static enum vctrs_class_type class_type_impl(SEXP class);
static const char* class_type_as_str(enum vctrs_class_type type);


// [[ register() ]]
SEXP vctrs_class_type(SEXP x) {
  return Rf_mkString(class_type_as_str(class_type(x)));
}


// [[ include("utils.h") ]]
enum vctrs_class_type class_type(SEXP x) {
  if (!OBJECT(x)) {
    return vctrs_class_none;
  }

  SEXP class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));

  // Avoid corrupt objects where `x` is an OBJECT(), but the class is NULL
  if (class == R_NilValue) {
    UNPROTECT(1);
    return vctrs_class_none;
  }

  enum vctrs_class_type type = class_type_impl(class);

  UNPROTECT(1);
  return type;
}

static enum vctrs_class_type class_type_impl(SEXP class) {
  int n = Rf_length(class);
  SEXP const* p = STRING_PTR_RO(class);

  // First check for bare types for which we know how many strings are
  // the classes composed of
  switch (n) {
  case 1: {
    SEXP p0 = p[0];

    if (p0 == strings_data_frame) {
      return vctrs_class_bare_data_frame;
    } else if (p0 == strings_factor) {
      return vctrs_class_bare_factor;
    } else if (p0 == strings_date) {
      return vctrs_class_bare_date;
    }

    break;
  }
  case 2: {
    SEXP p0 = p[0];
    SEXP p1 = p[1];

    if (p0 == strings_ordered &&
        p1 == strings_factor) {
      return vctrs_class_bare_ordered;
    }

    if (p1 == strings_posixt) {
      if (p0 == strings_posixct) {
        return vctrs_class_bare_posixct;
      } else if (p0 == strings_posixlt) {
        return vctrs_class_bare_posixlt;
      }
    }

    break;
  }
  case 3: {
    if (p[0] == strings_tbl_df &&
        p[1] == strings_tbl &&
        p[2] == strings_data_frame) {
      return vctrs_class_bare_tibble;
    }

    break;
  }}

  // Now check for inherited classes
  p = p + n - 1;
  SEXP last = *p;

  if (last == strings_data_frame) {
    return vctrs_class_data_frame;
  } else if (last == strings_list) {
    return vctrs_class_list;
  }

  return vctrs_class_unknown;
}

static const char* class_type_as_str(enum vctrs_class_type type) {
  switch (type) {
  case vctrs_class_list: return "list";
  case vctrs_class_data_frame: return "data_frame";
  case vctrs_class_bare_data_frame: return "bare_data_frame";
  case vctrs_class_bare_tibble: return "bare_tibble";
  case vctrs_class_bare_factor: return "bare_factor";
  case vctrs_class_bare_ordered: return "bare_ordered";
  case vctrs_class_bare_date: return "bare_date";
  case vctrs_class_bare_posixct: return "bare_posixct";
  case vctrs_class_bare_posixlt: return "bare_posixlt";
  case vctrs_class_unknown: return "unknown";
  case vctrs_class_none: return "none";
  }
  never_reached("class_type_as_str");
}


// [[ include("vctrs.h") ]]
bool vec_is_partial(SEXP x) {
  return x == R_NilValue || (TYPEOF(x) == VECSXP && Rf_inherits(x, "vctrs_partial"));
}

// [[ register() ]]
SEXP vctrs_is_partial(SEXP x) {
  return Rf_ScalarLogical(vec_is_partial(x));
}
