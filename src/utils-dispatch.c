#include "vctrs.h"
#include "decl/utils-dispatch-decl.h"


// [[ register() ]]
r_obj* ffi_class_type(r_obj* x) {
  return r_chr(class_type_as_str(class_type(x)));
}

enum vctrs_class_type class_type(r_obj* x) {
  if (!r_is_object(x)) {
    return VCTRS_CLASS_none;
  }

  r_obj* class = KEEP(r_class(x));

  // Avoid corrupt objects where `x` is an object, but the class is NULL
  if (class == r_null) {
    FREE(1);
    return VCTRS_CLASS_none;
  }

  enum vctrs_class_type type = class_type_impl(class);

  FREE(1);
  return type;
}

static
enum vctrs_class_type class_type_impl(r_obj* class) {
  int n = r_length(class);
  r_obj* const* p = r_chr_cbegin(class);

  // First check for bare types for which we know how many strings are
  // the classes composed of
  switch (n) {
  case 1: {
    r_obj* p0 = p[0];

    if (p0 == strings_data_frame) {
      return VCTRS_CLASS_bare_data_frame;
    } else if (p0 == strings_factor) {
      return VCTRS_CLASS_bare_factor;
    } else if (p0 == strings_date) {
      return VCTRS_CLASS_bare_date;
    } else if (p0 == strings.AsIs) {
      return VCTRS_CLASS_bare_asis;
    }

    break;
  }
  case 2: {
    r_obj* p0 = p[0];
    r_obj* p1 = p[1];

    if (p0 == strings_ordered &&
        p1 == strings_factor) {
      return VCTRS_CLASS_bare_ordered;
    }

    if (p1 == strings_posixt) {
      if (p0 == strings_posixct) {
        return VCTRS_CLASS_bare_posixct;
      } else if (p0 == strings_posixlt) {
        return VCTRS_CLASS_bare_posixlt;
      }
    }

    break;
  }
  case 3: {
    if (p[0] == strings_tbl_df &&
        p[1] == strings_tbl &&
        p[2] == strings_data_frame) {
      return VCTRS_CLASS_bare_tibble;
    }

    break;
  }}

  // Now check for inherited classes
  p = p + n - 1;
  r_obj* last = *p;

  if (last == strings_data_frame) {
    return VCTRS_CLASS_data_frame;
  } else if (last == strings_list) {
    return VCTRS_CLASS_list;
  }

  return VCTRS_CLASS_unknown;
}

bool class_type_is_data_frame(enum vctrs_class_type type) {
  switch (type) {
  case VCTRS_CLASS_data_frame:
  case VCTRS_CLASS_bare_data_frame:
  case VCTRS_CLASS_bare_tibble:
    return true;
  default:
    return false;
  }
}

static
const char* class_type_as_str(enum vctrs_class_type type) {
  switch (type) {
  case VCTRS_CLASS_list: return "list";
  case VCTRS_CLASS_data_frame: return "data_frame";
  case VCTRS_CLASS_bare_asis: return "bare_asis";
  case VCTRS_CLASS_bare_data_frame: return "bare_data_frame";
  case VCTRS_CLASS_bare_tibble: return "bare_tibble";
  case VCTRS_CLASS_bare_factor: return "bare_factor";
  case VCTRS_CLASS_bare_ordered: return "bare_ordered";
  case VCTRS_CLASS_bare_date: return "bare_date";
  case VCTRS_CLASS_bare_posixct: return "bare_posixct";
  case VCTRS_CLASS_bare_posixlt: return "bare_posixlt";
  case VCTRS_CLASS_unknown: return "unknown";
  case VCTRS_CLASS_none: return "none";
  }
  never_reached("class_type_as_str");
}


bool vec_is_partial(r_obj* x) {
  return x == r_null || (r_typeof(x) == R_TYPE_list && r_inherits(x, "vctrs_partial"));
}

// [[ register() ]]
r_obj* ffi_is_partial(r_obj* x) {
  return r_lgl(vec_is_partial(x));
}
