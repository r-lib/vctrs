#include "vctrs.h"


enum vctrs_dispatch vec_dispatch_typeof(SEXP x, SEXP y) {
  switch (vec_typeof(x)) {
  case vctrs_type_null: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_null;
    case vctrs_type_logical:   return vctrs_dispatch_null_logical;
    case vctrs_type_integer:   return vctrs_dispatch_null_integer;
    case vctrs_type_double:    return vctrs_dispatch_null_double;
    case vctrs_type_complex:   return vctrs_dispatch_null_complex;
    case vctrs_type_character: return vctrs_dispatch_null_character;
    case vctrs_type_raw:       return vctrs_dispatch_null_raw;
    case vctrs_type_list:      return vctrs_dispatch_null_list;
    case vctrs_type_dataframe: return vctrs_dispatch_null_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_null_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_null_scalar;
    }
  }
  case vctrs_type_logical: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_logical;
    case vctrs_type_logical:   return vctrs_dispatch_logical_logical;
    case vctrs_type_integer:   return vctrs_dispatch_logical_integer;
    case vctrs_type_double:    return vctrs_dispatch_logical_double;
    case vctrs_type_complex:   return vctrs_dispatch_logical_complex;
    case vctrs_type_character: return vctrs_dispatch_logical_character;
    case vctrs_type_raw:       return vctrs_dispatch_logical_raw;
    case vctrs_type_list:      return vctrs_dispatch_logical_list;
    case vctrs_type_dataframe: return vctrs_dispatch_logical_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_logical_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_logical_scalar;
    }
  }
  case vctrs_type_integer: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_integer;
    case vctrs_type_logical:   return vctrs_dispatch_logical_integer;
    case vctrs_type_integer:   return vctrs_dispatch_integer_integer;
    case vctrs_type_double:    return vctrs_dispatch_integer_double;
    case vctrs_type_complex:   return vctrs_dispatch_integer_complex;
    case vctrs_type_character: return vctrs_dispatch_integer_character;
    case vctrs_type_raw:       return vctrs_dispatch_integer_raw;
    case vctrs_type_list:      return vctrs_dispatch_integer_list;
    case vctrs_type_dataframe: return vctrs_dispatch_integer_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_integer_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_integer_scalar;
    }
  }
  case vctrs_type_double: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_double;
    case vctrs_type_logical:   return vctrs_dispatch_logical_double;
    case vctrs_type_integer:   return vctrs_dispatch_integer_double;
    case vctrs_type_double:    return vctrs_dispatch_double_double;
    case vctrs_type_complex:   return vctrs_dispatch_double_complex;
    case vctrs_type_character: return vctrs_dispatch_double_character;
    case vctrs_type_raw:       return vctrs_dispatch_double_raw;
    case vctrs_type_list:      return vctrs_dispatch_double_list;
    case vctrs_type_dataframe: return vctrs_dispatch_double_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_double_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_double_scalar;
    }
  }
  case vctrs_type_complex: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_complex;
    case vctrs_type_logical:   return vctrs_dispatch_logical_complex;
    case vctrs_type_integer:   return vctrs_dispatch_integer_complex;
    case vctrs_type_double:    return vctrs_dispatch_double_complex;
    case vctrs_type_complex:   return vctrs_dispatch_complex_complex;
    case vctrs_type_character: return vctrs_dispatch_complex_character;
    case vctrs_type_raw:       return vctrs_dispatch_complex_raw;
    case vctrs_type_list:      return vctrs_dispatch_complex_list;
    case vctrs_type_dataframe: return vctrs_dispatch_complex_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_complex_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_complex_scalar;
    }
  }
  case vctrs_type_character: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_character;
    case vctrs_type_logical:   return vctrs_dispatch_logical_character;
    case vctrs_type_integer:   return vctrs_dispatch_integer_character;
    case vctrs_type_double:    return vctrs_dispatch_double_character;
    case vctrs_type_complex:   return vctrs_dispatch_complex_character;
    case vctrs_type_character: return vctrs_dispatch_character_character;
    case vctrs_type_raw:       return vctrs_dispatch_character_raw;
    case vctrs_type_list:      return vctrs_dispatch_character_list;
    case vctrs_type_dataframe: return vctrs_dispatch_character_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_character_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_character_scalar;
    }
  }
  case vctrs_type_raw: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_raw;
    case vctrs_type_logical:   return vctrs_dispatch_logical_raw;
    case vctrs_type_integer:   return vctrs_dispatch_integer_raw;
    case vctrs_type_double:    return vctrs_dispatch_double_raw;
    case vctrs_type_complex:   return vctrs_dispatch_complex_raw;
    case vctrs_type_character: return vctrs_dispatch_character_raw;
    case vctrs_type_raw:       return vctrs_dispatch_raw_raw;
    case vctrs_type_list:      return vctrs_dispatch_raw_list;
    case vctrs_type_dataframe: return vctrs_dispatch_raw_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_raw_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_raw_scalar;
    }
  }
  case vctrs_type_list: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_list;
    case vctrs_type_logical:   return vctrs_dispatch_logical_list;
    case vctrs_type_integer:   return vctrs_dispatch_integer_list;
    case vctrs_type_double:    return vctrs_dispatch_double_list;
    case vctrs_type_complex:   return vctrs_dispatch_complex_list;
    case vctrs_type_character: return vctrs_dispatch_character_list;
    case vctrs_type_raw:       return vctrs_dispatch_raw_list;
    case vctrs_type_list:      return vctrs_dispatch_list_list;
    case vctrs_type_dataframe: return vctrs_dispatch_list_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_list_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_list_scalar;
    }
  }
  case vctrs_type_dataframe: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_dataframe;
    case vctrs_type_logical:   return vctrs_dispatch_logical_dataframe;
    case vctrs_type_integer:   return vctrs_dispatch_integer_dataframe;
    case vctrs_type_double:    return vctrs_dispatch_double_dataframe;
    case vctrs_type_complex:   return vctrs_dispatch_complex_dataframe;
    case vctrs_type_character: return vctrs_dispatch_character_dataframe;
    case vctrs_type_raw:       return vctrs_dispatch_raw_dataframe;
    case vctrs_type_list:      return vctrs_dispatch_list_dataframe;
    case vctrs_type_dataframe: return vctrs_dispatch_dataframe_dataframe;
    case vctrs_type_s3:        return vctrs_dispatch_dataframe_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_dataframe_scalar;
    }
  }
  case vctrs_type_s3: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_s3;
    case vctrs_type_logical:   return vctrs_dispatch_logical_s3;
    case vctrs_type_integer:   return vctrs_dispatch_integer_s3;
    case vctrs_type_double:    return vctrs_dispatch_double_s3;
    case vctrs_type_complex:   return vctrs_dispatch_complex_s3;
    case vctrs_type_character: return vctrs_dispatch_character_s3;
    case vctrs_type_raw:       return vctrs_dispatch_raw_s3;
    case vctrs_type_list:      return vctrs_dispatch_list_s3;
    case vctrs_type_dataframe: return vctrs_dispatch_dataframe_s3;
    case vctrs_type_s3:        return vctrs_dispatch_s3_s3;
    case vctrs_type_scalar:    return vctrs_dispatch_s3_scalar;
    }
  }
  case vctrs_type_scalar: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_dispatch_null_scalar;
    case vctrs_type_logical:   return vctrs_dispatch_logical_scalar;
    case vctrs_type_integer:   return vctrs_dispatch_integer_scalar;
    case vctrs_type_double:    return vctrs_dispatch_double_scalar;
    case vctrs_type_complex:   return vctrs_dispatch_complex_scalar;
    case vctrs_type_character: return vctrs_dispatch_character_scalar;
    case vctrs_type_raw:       return vctrs_dispatch_raw_scalar;
    case vctrs_type_list:      return vctrs_dispatch_list_scalar;
    case vctrs_type_dataframe: return vctrs_dispatch_dataframe_scalar;
    case vctrs_type_s3:        return vctrs_dispatch_s3_scalar;
    case vctrs_type_scalar:    return vctrs_dispatch_scalar_scalar;
    }
  }
  default:
    Rf_errorcall(R_NilValue, "Unimplemented type in `vec_dispatch_type()`");
  }
};

const char* vctrs_dispatch_type_as_str(enum vctrs_dispatch type) {
  switch (type) {
  case vctrs_dispatch_null_null:           return "vctrs_dispatch_null_null";
  case vctrs_dispatch_null_logical:        return "vctrs_dispatch_null_logical";
  case vctrs_dispatch_null_integer:        return "vctrs_dispatch_null_integer";
  case vctrs_dispatch_null_double:         return "vctrs_dispatch_null_double";
  case vctrs_dispatch_null_complex:        return "vctrs_dispatch_null_complex";
  case vctrs_dispatch_null_character:      return "vctrs_dispatch_null_character";
  case vctrs_dispatch_null_raw:            return "vctrs_dispatch_null_raw";
  case vctrs_dispatch_null_list:           return "vctrs_dispatch_null_list";
  case vctrs_dispatch_null_dataframe:      return "vctrs_dispatch_null_dataframe";
  case vctrs_dispatch_null_s3:             return "vctrs_dispatch_null_s3";
  case vctrs_dispatch_null_scalar:         return "vctrs_dispatch_null_scalar";

  case vctrs_dispatch_logical_logical:     return "vctrs_dispatch_logical_logical";
  case vctrs_dispatch_logical_integer:     return "vctrs_dispatch_logical_integer";
  case vctrs_dispatch_logical_double:      return "vctrs_dispatch_logical_double";
  case vctrs_dispatch_logical_complex:     return "vctrs_dispatch_logical_complex";
  case vctrs_dispatch_logical_character:   return "vctrs_dispatch_logical_character";
  case vctrs_dispatch_logical_raw:         return "vctrs_dispatch_logical_raw";
  case vctrs_dispatch_logical_list:        return "vctrs_dispatch_logical_list";
  case vctrs_dispatch_logical_dataframe:   return "vctrs_dispatch_logical_dataframe";
  case vctrs_dispatch_logical_s3:          return "vctrs_dispatch_logical_s3";
  case vctrs_dispatch_logical_scalar:      return "vctrs_dispatch_logical_scalar";

  case vctrs_dispatch_integer_integer:     return "vctrs_dispatch_integer_integer";
  case vctrs_dispatch_integer_double:      return "vctrs_dispatch_integer_double";
  case vctrs_dispatch_integer_complex:     return "vctrs_dispatch_integer_complex";
  case vctrs_dispatch_integer_character:   return "vctrs_dispatch_integer_character";
  case vctrs_dispatch_integer_raw:         return "vctrs_dispatch_integer_raw";
  case vctrs_dispatch_integer_list:        return "vctrs_dispatch_integer_list";
  case vctrs_dispatch_integer_dataframe:   return "vctrs_dispatch_integer_dataframe";
  case vctrs_dispatch_integer_s3:          return "vctrs_dispatch_integer_s3";
  case vctrs_dispatch_integer_scalar:      return "vctrs_dispatch_integer_scalar";

  case vctrs_dispatch_double_double:       return "vctrs_dispatch_double_double";
  case vctrs_dispatch_double_complex:      return "vctrs_dispatch_double_complex";
  case vctrs_dispatch_double_character:    return "vctrs_dispatch_double_character";
  case vctrs_dispatch_double_raw:          return "vctrs_dispatch_double_raw";
  case vctrs_dispatch_double_list:         return "vctrs_dispatch_double_list";
  case vctrs_dispatch_double_dataframe:    return "vctrs_dispatch_double_dataframe";
  case vctrs_dispatch_double_s3:           return "vctrs_dispatch_double_s3";
  case vctrs_dispatch_double_scalar:       return "vctrs_dispatch_double_scalar";

  case vctrs_dispatch_complex_complex:     return "vctrs_dispatch_complex_complex";
  case vctrs_dispatch_complex_character:   return "vctrs_dispatch_complex_character";
  case vctrs_dispatch_complex_raw:         return "vctrs_dispatch_complex_raw";
  case vctrs_dispatch_complex_list:        return "vctrs_dispatch_complex_list";
  case vctrs_dispatch_complex_dataframe:   return "vctrs_dispatch_complex_dataframe";
  case vctrs_dispatch_complex_s3:          return "vctrs_dispatch_complex_s3";
  case vctrs_dispatch_complex_scalar:      return "vctrs_dispatch_complex_scalar";

  case vctrs_dispatch_character_character: return "vctrs_dispatch_character_character";
  case vctrs_dispatch_character_raw:       return "vctrs_dispatch_character_raw";
  case vctrs_dispatch_character_list:      return "vctrs_dispatch_character_list";
  case vctrs_dispatch_character_dataframe: return "vctrs_dispatch_character_dataframe";
  case vctrs_dispatch_character_s3:        return "vctrs_dispatch_character_s3";
  case vctrs_dispatch_character_scalar:    return "vctrs_dispatch_character_scalar";

  case vctrs_dispatch_raw_raw:             return "vctrs_dispatch_raw_raw";
  case vctrs_dispatch_raw_list:            return "vctrs_dispatch_raw_list";
  case vctrs_dispatch_raw_dataframe:       return "vctrs_dispatch_raw_dataframe";
  case vctrs_dispatch_raw_s3:              return "vctrs_dispatch_raw_s3";
  case vctrs_dispatch_raw_scalar:          return "vctrs_dispatch_raw_scalar";

  case vctrs_dispatch_list_list:           return "vctrs_dispatch_list_list";
  case vctrs_dispatch_list_dataframe:      return "vctrs_dispatch_list_dataframe";
  case vctrs_dispatch_list_s3:             return "vctrs_dispatch_list_s3";
  case vctrs_dispatch_list_scalar:         return "vctrs_dispatch_list_scalar";

  case vctrs_dispatch_dataframe_dataframe: return "vctrs_dispatch_dataframe_dataframe";
  case vctrs_dispatch_dataframe_s3:        return "vctrs_dispatch_dataframe_s3";
  case vctrs_dispatch_dataframe_scalar:    return "vctrs_dispatch_dataframe_scalar";

  case vctrs_dispatch_s3_s3:               return "vctrs_dispatch_s3_s3";
  case vctrs_dispatch_s3_scalar:           return "vctrs_dispatch_s3_scalar";

  case vctrs_dispatch_scalar_scalar:       return "vctrs_dispatch_scalar_scalar";
  }
}

SEXP vctrs_dispatch_typeof(SEXP x, SEXP y) {
  enum vctrs_dispatch type = vec_dispatch_typeof(x, y);
  return Rf_mkString(vctrs_dispatch_type_as_str(type));
}
