#include "vctrs.h"
#include "utils.h"

static enum vctrs_type2_s3 vec_typeof2_s3_impl2(SEXP x,
                                                SEXP y,
                                                enum vctrs_type type_y,
                                                int* left);

// [[ include("vctrs.h") ]]
enum vctrs_type2_s3 vec_typeof2_s3_impl(SEXP x,
                                        SEXP y,
                                        enum vctrs_type type_x,
                                        enum vctrs_type type_y,
                                        int* left) {
  switch (type_x) {
  case vctrs_type_null: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_null_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_null_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_null_unknown;
    }
  }
  case vctrs_type_logical: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_logical_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_logical_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_logical_unknown;
    }
  }
  case vctrs_type_integer: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_integer_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_integer_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_integer_unknown;
    }
  }
  case vctrs_type_double: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_double_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_double_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_double_unknown;
    }
  }
  case vctrs_type_complex: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_complex_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_complex_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_complex_unknown;
    }
  }
  case vctrs_type_character: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_character_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_character_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_character_unknown;
    }
  }
  case vctrs_type_raw: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_raw_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_raw_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_raw_unknown;
    }
  }
  case vctrs_type_list: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_list_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_list_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_list_unknown;
    }
  }
  case vctrs_type_dataframe: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_dataframe_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_dataframe_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_dataframe_unknown;
    }
  }
  case vctrs_type_scalar: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_scalar_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_scalar_bare_ordered;
    default:                       *left = 0; return vctrs_type2_s3_scalar_unknown;
    }
  }
  case vctrs_type_s3: {
    return vec_typeof2_s3_impl2(x, y, type_y, left);
  }}

  never_reached("vec_typeof2_s3_impl()");
}


static enum vctrs_type2_s3 vec_typeof2_s3_impl2(SEXP x,
                                                SEXP y,
                                                enum vctrs_type type_y,
                                                int* left) {
  switch (class_type(x)) {
  case vctrs_class_bare_factor: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_bare_factor;
    case vctrs_type_logical:         *left =  1; return vctrs_type2_s3_logical_bare_factor;
    case vctrs_type_integer:         *left =  1; return vctrs_type2_s3_integer_bare_factor;
    case vctrs_type_double:          *left =  1; return vctrs_type2_s3_double_bare_factor;
    case vctrs_type_complex:         *left =  1; return vctrs_type2_s3_complex_bare_factor;
    case vctrs_type_character:       *left =  1; return vctrs_type2_s3_character_bare_factor;
    case vctrs_type_raw:             *left =  1; return vctrs_type2_s3_raw_bare_factor;
    case vctrs_type_list:            *left =  1; return vctrs_type2_s3_list_bare_factor;
    case vctrs_type_dataframe:       *left =  1; return vctrs_type2_s3_dataframe_bare_factor;
    case vctrs_type_scalar:          *left =  1; return vctrs_type2_s3_scalar_bare_factor;
    case vctrs_type_s3: {
      switch (class_type(y)) {
      case vctrs_class_bare_factor:  *left = -1; return vctrs_type2_s3_bare_factor_bare_factor;
      case vctrs_class_bare_ordered: *left =  0; return vctrs_type2_s3_bare_factor_bare_ordered;
      default:                       *left =  0; return vctrs_type2_s3_bare_factor_unknown;
      }
    }}
  }
  case vctrs_class_bare_ordered: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_bare_ordered;
    case vctrs_type_logical:         *left =  1; return vctrs_type2_s3_logical_bare_ordered;
    case vctrs_type_integer:         *left =  1; return vctrs_type2_s3_integer_bare_ordered;
    case vctrs_type_double:          *left =  1; return vctrs_type2_s3_double_bare_ordered;
    case vctrs_type_complex:         *left =  1; return vctrs_type2_s3_complex_bare_ordered;
    case vctrs_type_character:       *left =  1; return vctrs_type2_s3_character_bare_ordered;
    case vctrs_type_raw:             *left =  1; return vctrs_type2_s3_raw_bare_ordered;
    case vctrs_type_list:            *left =  1; return vctrs_type2_s3_list_bare_ordered;
    case vctrs_type_dataframe:       *left =  1; return vctrs_type2_s3_dataframe_bare_ordered;
    case vctrs_type_scalar:          *left =  1; return vctrs_type2_s3_scalar_bare_ordered;
    case vctrs_type_s3: {
      switch (class_type(y)) {
      case vctrs_class_bare_factor:  *left =  1; return vctrs_type2_s3_bare_factor_bare_ordered;
      case vctrs_class_bare_ordered: *left = -1; return vctrs_type2_s3_bare_ordered_bare_ordered;
      default:                       *left =  0; return vctrs_type2_s3_bare_ordered_unknown;
      }
    }}
  }
  default: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_unknown;
    case vctrs_type_logical:         *left =  1; return vctrs_type2_s3_logical_unknown;
    case vctrs_type_integer:         *left =  1; return vctrs_type2_s3_integer_unknown;
    case vctrs_type_double:          *left =  1; return vctrs_type2_s3_double_unknown;
    case vctrs_type_complex:         *left =  1; return vctrs_type2_s3_complex_unknown;
    case vctrs_type_character:       *left =  1; return vctrs_type2_s3_character_unknown;
    case vctrs_type_raw:             *left =  1; return vctrs_type2_s3_raw_unknown;
    case vctrs_type_list:            *left =  1; return vctrs_type2_s3_list_unknown;
    case vctrs_type_dataframe:       *left =  1; return vctrs_type2_s3_dataframe_unknown;
    case vctrs_type_scalar:          *left =  1; return vctrs_type2_s3_scalar_unknown;
    case vctrs_type_s3: {
      switch (class_type(y)) {
      case vctrs_class_bare_factor:  *left =  1; return vctrs_type2_s3_bare_factor_unknown;
      case vctrs_class_bare_ordered: *left =  1; return vctrs_type2_s3_bare_ordered_unknown;
      default:                       *left = -1; return vctrs_type2_s3_unknown_unknown;
      }
    }}
  }}

  never_reached("vec_typeof2_s3_impl2()");
}

enum vctrs_type2_s3 vec_typeof2_s3(SEXP x, SEXP y) {
  int _;
  return vec_typeof2_s3_impl(x, y, vec_typeof(x), vec_typeof(y), &_);
}

const char* vctrs_type2_s3_as_str(enum vctrs_type2_s3 type) {
  switch (type) {
  case vctrs_type2_s3_null_bare_factor:            return "vctrs_type2_s3_null_bare_factor";
  case vctrs_type2_s3_null_bare_ordered:           return "vctrs_type2_s3_null_bare_ordered";
  case vctrs_type2_s3_null_unknown:                return "vctrs_type2_s3_null_unknown";

  case vctrs_type2_s3_logical_bare_factor:         return "vctrs_type2_s3_logical_bare_factor";
  case vctrs_type2_s3_logical_bare_ordered:        return "vctrs_type2_s3_logical_bare_ordered";
  case vctrs_type2_s3_logical_unknown:             return "vctrs_type2_s3_logical_unknown";

  case vctrs_type2_s3_integer_bare_factor:         return "vctrs_type2_s3_integer_bare_factor";
  case vctrs_type2_s3_integer_bare_ordered:        return "vctrs_type2_s3_integer_bare_ordered";
  case vctrs_type2_s3_integer_unknown:             return "vctrs_type2_s3_integer_unknown";

  case vctrs_type2_s3_double_bare_factor:          return "vctrs_type2_s3_double_bare_factor";
  case vctrs_type2_s3_double_bare_ordered:         return "vctrs_type2_s3_double_bare_ordered";
  case vctrs_type2_s3_double_unknown:              return "vctrs_type2_s3_double_unknown";

  case vctrs_type2_s3_complex_bare_factor:         return "vctrs_type2_s3_complex_bare_factor";
  case vctrs_type2_s3_complex_bare_ordered:        return "vctrs_type2_s3_complex_bare_ordered";
  case vctrs_type2_s3_complex_unknown:             return "vctrs_type2_s3_complex_unknown";

  case vctrs_type2_s3_character_bare_factor:       return "vctrs_type2_s3_character_bare_factor";
  case vctrs_type2_s3_character_bare_ordered:      return "vctrs_type2_s3_character_bare_ordered";
  case vctrs_type2_s3_character_unknown:           return "vctrs_type2_s3_character_unknown";

  case vctrs_type2_s3_raw_bare_factor:             return "vctrs_type2_s3_raw_bare_factor";
  case vctrs_type2_s3_raw_bare_ordered:            return "vctrs_type2_s3_raw_bare_ordered";
  case vctrs_type2_s3_raw_unknown:                 return "vctrs_type2_s3_raw_unknown";

  case vctrs_type2_s3_list_bare_factor:            return "vctrs_type2_s3_list_bare_factor";
  case vctrs_type2_s3_list_bare_ordered:           return "vctrs_type2_s3_list_bare_ordered";
  case vctrs_type2_s3_list_unknown:                return "vctrs_type2_s3_list_unknown";

  case vctrs_type2_s3_dataframe_bare_factor:       return "vctrs_type2_s3_dataframe_bare_factor";
  case vctrs_type2_s3_dataframe_bare_ordered:      return "vctrs_type2_s3_dataframe_bare_ordered";
  case vctrs_type2_s3_dataframe_unknown:           return "vctrs_type2_s3_dataframe_unknown";

  case vctrs_type2_s3_scalar_bare_factor:          return "vctrs_type2_s3_scalar_bare_factor";
  case vctrs_type2_s3_scalar_bare_ordered:         return "vctrs_type2_s3_scalar_bare_ordered";
  case vctrs_type2_s3_scalar_unknown:              return "vctrs_type2_s3_scalar_unknown";

  case vctrs_type2_s3_bare_factor_bare_factor:     return "vctrs_type2_s3_bare_factor_bare_factor";
  case vctrs_type2_s3_bare_factor_bare_ordered:    return "vctrs_type2_s3_bare_factor_bare_ordered";
  case vctrs_type2_s3_bare_factor_unknown:         return "vctrs_type2_s3_bare_factor_unknown";

  case vctrs_type2_s3_bare_ordered_bare_ordered:   return "vctrs_type2_s3_bare_ordered_bare_ordered";
  case vctrs_type2_s3_bare_ordered_unknown:        return "vctrs_type2_s3_bare_ordered_unknown";

  case vctrs_type2_s3_unknown_unknown:             return "vctrs_type2_s3_unknown_unknown";
  }

  never_reached("vctrs_type2_s3_as_str");
}

// [[ register() ]]
SEXP vctrs_typeof2_s3(SEXP x, SEXP y) {
  enum vctrs_type2_s3 type = vec_typeof2_s3(x, y);
  return Rf_mkString(vctrs_type2_s3_as_str(type));
}
