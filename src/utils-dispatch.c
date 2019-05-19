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
bool is_data_frame(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble ||
    type == vctrs_class_data_frame;
}

// [[ include("utils.h") ]]
bool is_native_df(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble;
}

// [[ include("utils.h") ]]
bool is_bare_data_frame(SEXP x) {
  return class_type(x) == vctrs_class_bare_data_frame;
}

// [[ include("utils.h") ]]
bool is_bare_tibble(SEXP x) {
  return class_type(x) == vctrs_class_bare_tibble;
}

// [[ include("utils.h") ]]
bool is_record(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_rcrd ||
    type == vctrs_class_posixlt;
}


enum vctrs_class_type class_type(SEXP x) {
  if (!OBJECT(x)) {
    return vctrs_class_none;
  }

  SEXP class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  enum vctrs_class_type type = class_type_impl(class);

  UNPROTECT(1);
  return type;
}

static enum vctrs_class_type class_type_impl(SEXP class) {
  int n = Rf_length(class);
  SEXP const* class_ptr = STRING_PTR(class);
  SEXP const* p = class_ptr;

  // First check for bare types for which we know how many strings are
  // the classes composed of
  switch (n) {
  case 1:
    if (*p != strings_data_frame) break;
    return vctrs_class_bare_data_frame;
  case 3: {
    if (*p++ != strings_tbl_df) break;
    if (*p++ != strings_tbl) break;
    if (*p++ != strings_data_frame) break;
    return vctrs_class_bare_tibble;
  }}

  // Now check for inherited classes
  p = class_ptr + n - 2;
  SEXP butlast = *p++;
  SEXP last = *p++;

  if (butlast == strings_posixlt) {
    if (last == strings_posixt) return vctrs_class_posixlt;
  } else if (butlast == strings_vctrs_rcrd) {
    if (last == strings_vctrs_vctr) return vctrs_class_rcrd;
  } else if (last == strings_data_frame) {
    return vctrs_class_data_frame;
  }

  return vctrs_class_unknown;
}

static const char* class_type_as_str(enum vctrs_class_type type) {
  switch (type) {
  case vctrs_class_data_frame: return "data_frame";
  case vctrs_class_bare_data_frame: return "bare_data_frame";
  case vctrs_class_bare_tibble: return "bare_tibble";
  case vctrs_class_rcrd: return "rcrd";
  case vctrs_class_posixlt: return "posixlt";
  case vctrs_class_unknown: return "unknown";
  case vctrs_class_none: return "none";
  }
  never_reached("class_type_as_str");
}


// [[ include("vctrs.h") ]]
bool vec_is_partial(SEXP x) {
  return x == R_NilValue || Rf_inherits(x, "vctrs_partial");
}
