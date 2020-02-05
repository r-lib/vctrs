#include "vctrs.h"
#include "utils.h"

static enum vctrs_s3_type2 vec_s3_typeof2_impl2(SEXP x,
                                                SEXP y,
                                                enum vctrs_type type_y,
                                                int* left);

// [[ include("vctrs.h") ]]
enum vctrs_s3_type2 vec_s3_typeof2_impl(SEXP x,
                                        SEXP y,
                                        enum vctrs_type type_x,
                                        enum vctrs_type type_y,
                                        int* left) {
  switch(type_x) {
  case vctrs_type_null: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_null_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_null_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_null_unknown;
    }
  }
  case vctrs_type_logical: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_logical_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_logical_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_logical_unknown;
    }
  }
  case vctrs_type_integer: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_integer_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_integer_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_integer_unknown;
    }
  }
  case vctrs_type_double: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_double_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_double_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_double_unknown;
    }
  }
  case vctrs_type_complex: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_complex_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_complex_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_complex_unknown;
    }
  }
  case vctrs_type_character: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_character_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_character_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_character_unknown;
    }
  }
  case vctrs_type_raw: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_raw_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_raw_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_raw_unknown;
    }
  }
  case vctrs_type_list: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_list_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_list_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_list_unknown;
    }
  }
  case vctrs_type_dataframe: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_dataframe_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_dataframe_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_dataframe_unknown;
    }
  }
  case vctrs_type_scalar: {
    switch(s3_class_type(y)) {
    case vctrs_s3_class_bare_factor:  *left = 0; return vctrs_s3_type2_scalar_bare_factor;
    case vctrs_s3_class_bare_ordered: *left = 0; return vctrs_s3_type2_scalar_bare_ordered;
    case vctrs_s3_class_unknown:      *left = 0; return vctrs_s3_type2_scalar_unknown;
    }
  }
  case vctrs_type_s3: {
    return vec_s3_typeof2_impl2(x, y, type_y, left);
  }
  }

  never_reached("vec_s3_typeof2_impl()");
}


static enum vctrs_s3_type2 vec_s3_typeof2_impl2(SEXP x,
                                                SEXP y,
                                                enum vctrs_type type_y,
                                                int* left) {
  switch(s3_class_type(x)) {
  case vctrs_s3_class_bare_factor: {
    switch(type_y) {
    case vctrs_type_null:               *left =  1; return vctrs_s3_type2_null_bare_factor;
    case vctrs_type_logical:            *left =  1; return vctrs_s3_type2_logical_bare_factor;
    case vctrs_type_integer:            *left =  1; return vctrs_s3_type2_integer_bare_factor;
    case vctrs_type_double:             *left =  1; return vctrs_s3_type2_double_bare_factor;
    case vctrs_type_complex:            *left =  1; return vctrs_s3_type2_complex_bare_factor;
    case vctrs_type_character:          *left =  1; return vctrs_s3_type2_character_bare_factor;
    case vctrs_type_raw:                *left =  1; return vctrs_s3_type2_raw_bare_factor;
    case vctrs_type_list:               *left =  1; return vctrs_s3_type2_list_bare_factor;
    case vctrs_type_dataframe:          *left =  1; return vctrs_s3_type2_dataframe_bare_factor;
    case vctrs_type_scalar:             *left =  1; return vctrs_s3_type2_scalar_bare_factor;
    case vctrs_type_s3: {
      switch(s3_class_type(y)) {
      case vctrs_s3_class_bare_factor:  *left = -1; return vctrs_s3_type2_bare_factor_bare_factor;
      case vctrs_s3_class_bare_ordered: *left =  0; return vctrs_s3_type2_bare_factor_bare_ordered;
      case vctrs_s3_class_unknown:      *left =  0; return vctrs_s3_type2_bare_factor_unknown;
      }
    }
    }
  }
  case vctrs_s3_class_bare_ordered: {
    switch(type_y) {
    case vctrs_type_null:               *left =  1; return vctrs_s3_type2_null_bare_ordered;
    case vctrs_type_logical:            *left =  1; return vctrs_s3_type2_logical_bare_ordered;
    case vctrs_type_integer:            *left =  1; return vctrs_s3_type2_integer_bare_ordered;
    case vctrs_type_double:             *left =  1; return vctrs_s3_type2_double_bare_ordered;
    case vctrs_type_complex:            *left =  1; return vctrs_s3_type2_complex_bare_ordered;
    case vctrs_type_character:          *left =  1; return vctrs_s3_type2_character_bare_ordered;
    case vctrs_type_raw:                *left =  1; return vctrs_s3_type2_raw_bare_ordered;
    case vctrs_type_list:               *left =  1; return vctrs_s3_type2_list_bare_ordered;
    case vctrs_type_dataframe:          *left =  1; return vctrs_s3_type2_dataframe_bare_ordered;
    case vctrs_type_scalar:             *left =  1; return vctrs_s3_type2_scalar_bare_ordered;
    case vctrs_type_s3: {
      switch(s3_class_type(y)) {
      case vctrs_s3_class_bare_factor:  *left =  1; return vctrs_s3_type2_bare_factor_bare_ordered;
      case vctrs_s3_class_bare_ordered: *left = -1; return vctrs_s3_type2_bare_ordered_bare_ordered;
      case vctrs_s3_class_unknown:      *left =  0; return vctrs_s3_type2_bare_ordered_unknown;
      }
    }
    }
  }
  case vctrs_s3_class_unknown: {
    switch(type_y) {
    case vctrs_type_null:               *left =  1; return vctrs_s3_type2_null_unknown;
    case vctrs_type_logical:            *left =  1; return vctrs_s3_type2_logical_unknown;
    case vctrs_type_integer:            *left =  1; return vctrs_s3_type2_integer_unknown;
    case vctrs_type_double:             *left =  1; return vctrs_s3_type2_double_unknown;
    case vctrs_type_complex:            *left =  1; return vctrs_s3_type2_complex_unknown;
    case vctrs_type_character:          *left =  1; return vctrs_s3_type2_character_unknown;
    case vctrs_type_raw:                *left =  1; return vctrs_s3_type2_raw_unknown;
    case vctrs_type_list:               *left =  1; return vctrs_s3_type2_list_unknown;
    case vctrs_type_dataframe:          *left =  1; return vctrs_s3_type2_dataframe_unknown;
    case vctrs_type_scalar:             *left =  1; return vctrs_s3_type2_scalar_unknown;
    case vctrs_type_s3: {
      switch(s3_class_type(y)) {
      case vctrs_s3_class_bare_factor:  *left =  1; return vctrs_s3_type2_bare_factor_unknown;
      case vctrs_s3_class_bare_ordered: *left =  1; return vctrs_s3_type2_bare_ordered_unknown;
      case vctrs_s3_class_unknown:      *left = -1; return vctrs_s3_type2_unknown_unknown;
      }
    }
    }
  }
  }

  never_reached("vec_s3_typeof2_impl2()");
}

enum vctrs_s3_type2 vec_s3_typeof2(SEXP x, SEXP y) {
  int _;
  return vec_s3_typeof2_impl(x, y, vec_typeof(x), vec_typeof(y), &_);
}

const char* vctrs_s3_type2_as_str(enum vctrs_s3_type2 type) {
  switch(type) {
  case vctrs_s3_type2_null_bare_factor:            return "vctrs_s3_type2_null_bare_factor";
  case vctrs_s3_type2_null_bare_ordered:           return "vctrs_s3_type2_null_bare_ordered";
  case vctrs_s3_type2_null_unknown:                return "vctrs_s3_type2_null_unknown";

  case vctrs_s3_type2_logical_bare_factor:         return "vctrs_s3_type2_logical_bare_factor";
  case vctrs_s3_type2_logical_bare_ordered:        return "vctrs_s3_type2_logical_bare_ordered";
  case vctrs_s3_type2_logical_unknown:             return "vctrs_s3_type2_logical_unknown";

  case vctrs_s3_type2_integer_bare_factor:         return "vctrs_s3_type2_integer_bare_factor";
  case vctrs_s3_type2_integer_bare_ordered:        return "vctrs_s3_type2_integer_bare_ordered";
  case vctrs_s3_type2_integer_unknown:             return "vctrs_s3_type2_integer_unknown";

  case vctrs_s3_type2_double_bare_factor:          return "vctrs_s3_type2_double_bare_factor";
  case vctrs_s3_type2_double_bare_ordered:         return "vctrs_s3_type2_double_bare_ordered";
  case vctrs_s3_type2_double_unknown:              return "vctrs_s3_type2_double_unknown";

  case vctrs_s3_type2_complex_bare_factor:         return "vctrs_s3_type2_complex_bare_factor";
  case vctrs_s3_type2_complex_bare_ordered:        return "vctrs_s3_type2_complex_bare_ordered";
  case vctrs_s3_type2_complex_unknown:             return "vctrs_s3_type2_complex_unknown";

  case vctrs_s3_type2_character_bare_factor:       return "vctrs_s3_type2_character_bare_factor";
  case vctrs_s3_type2_character_bare_ordered:      return "vctrs_s3_type2_character_bare_ordered";
  case vctrs_s3_type2_character_unknown:           return "vctrs_s3_type2_character_unknown";

  case vctrs_s3_type2_raw_bare_factor:             return "vctrs_s3_type2_raw_bare_factor";
  case vctrs_s3_type2_raw_bare_ordered:            return "vctrs_s3_type2_raw_bare_ordered";
  case vctrs_s3_type2_raw_unknown:                 return "vctrs_s3_type2_raw_unknown";

  case vctrs_s3_type2_list_bare_factor:            return "vctrs_s3_type2_list_bare_factor";
  case vctrs_s3_type2_list_bare_ordered:           return "vctrs_s3_type2_list_bare_ordered";
  case vctrs_s3_type2_list_unknown:                return "vctrs_s3_type2_list_unknown";

  case vctrs_s3_type2_dataframe_bare_factor:       return "vctrs_s3_type2_dataframe_bare_factor";
  case vctrs_s3_type2_dataframe_bare_ordered:      return "vctrs_s3_type2_dataframe_bare_ordered";
  case vctrs_s3_type2_dataframe_unknown:           return "vctrs_s3_type2_dataframe_unknown";

  case vctrs_s3_type2_scalar_bare_factor:          return "vctrs_s3_type2_scalar_bare_factor";
  case vctrs_s3_type2_scalar_bare_ordered:         return "vctrs_s3_type2_scalar_bare_ordered";
  case vctrs_s3_type2_scalar_unknown:              return "vctrs_s3_type2_scalar_unknown";

  case vctrs_s3_type2_bare_factor_bare_factor:     return "vctrs_s3_type2_bare_factor_bare_factor";
  case vctrs_s3_type2_bare_factor_bare_ordered:    return "vctrs_s3_type2_bare_factor_bare_ordered";
  case vctrs_s3_type2_bare_factor_unknown:         return "vctrs_s3_type2_bare_factor_unknown";

  case vctrs_s3_type2_bare_ordered_bare_ordered:   return "vctrs_s3_type2_bare_ordered_bare_ordered";
  case vctrs_s3_type2_bare_ordered_unknown:        return "vctrs_s3_type2_bare_ordered_unknown";

  case vctrs_s3_type2_unknown_unknown:             return "vctrs_s3_type2_unknown_unknown";
  }
}

// [[ register() ]]
SEXP vctrs_s3_typeof2(SEXP x, SEXP y) {
  enum vctrs_s3_type2 type = vec_s3_typeof2(x, y);
  return Rf_mkString(vctrs_s3_type2_as_str(type));
}
