#include "vctrs.h"

/**
 * Type for symmetric binary dispatch.
 *
 * Permuting `x` and `y` does not change the typeof2.
 *
 * After adding entries in `vec_typeof2()`, adjust the list of types
 * in helper-types.R. This will ensure the consistency of the new
 * entries.
 */
enum vctrs_type2 vec_typeof2(SEXP x, SEXP y) {
  switch (vec_typeof(x)) {
  case vctrs_type_null: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_null;
    case vctrs_type_logical:   return vctrs_type2_null_logical;
    case vctrs_type_integer:   return vctrs_type2_null_integer;
    case vctrs_type_double:    return vctrs_type2_null_double;
    case vctrs_type_complex:   return vctrs_type2_null_complex;
    case vctrs_type_character: return vctrs_type2_null_character;
    case vctrs_type_raw:       return vctrs_type2_null_raw;
    case vctrs_type_list:      return vctrs_type2_null_list;
    case vctrs_type_dataframe: return vctrs_type2_null_dataframe;
    case vctrs_type_s3:        return vctrs_type2_null_s3;
    case vctrs_type_scalar:    return vctrs_type2_null_scalar;
    }
  }
  case vctrs_type_logical: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_logical;
    case vctrs_type_logical:   return vctrs_type2_logical_logical;
    case vctrs_type_integer:   return vctrs_type2_logical_integer;
    case vctrs_type_double:    return vctrs_type2_logical_double;
    case vctrs_type_complex:   return vctrs_type2_logical_complex;
    case vctrs_type_character: return vctrs_type2_logical_character;
    case vctrs_type_raw:       return vctrs_type2_logical_raw;
    case vctrs_type_list:      return vctrs_type2_logical_list;
    case vctrs_type_dataframe: return vctrs_type2_logical_dataframe;
    case vctrs_type_s3:        return vctrs_type2_logical_s3;
    case vctrs_type_scalar:    return vctrs_type2_logical_scalar;
    }
  }
  case vctrs_type_integer: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_integer;
    case vctrs_type_logical:   return vctrs_type2_logical_integer;
    case vctrs_type_integer:   return vctrs_type2_integer_integer;
    case vctrs_type_double:    return vctrs_type2_integer_double;
    case vctrs_type_complex:   return vctrs_type2_integer_complex;
    case vctrs_type_character: return vctrs_type2_integer_character;
    case vctrs_type_raw:       return vctrs_type2_integer_raw;
    case vctrs_type_list:      return vctrs_type2_integer_list;
    case vctrs_type_dataframe: return vctrs_type2_integer_dataframe;
    case vctrs_type_s3:        return vctrs_type2_integer_s3;
    case vctrs_type_scalar:    return vctrs_type2_integer_scalar;
    }
  }
  case vctrs_type_double: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_double;
    case vctrs_type_logical:   return vctrs_type2_logical_double;
    case vctrs_type_integer:   return vctrs_type2_integer_double;
    case vctrs_type_double:    return vctrs_type2_double_double;
    case vctrs_type_complex:   return vctrs_type2_double_complex;
    case vctrs_type_character: return vctrs_type2_double_character;
    case vctrs_type_raw:       return vctrs_type2_double_raw;
    case vctrs_type_list:      return vctrs_type2_double_list;
    case vctrs_type_dataframe: return vctrs_type2_double_dataframe;
    case vctrs_type_s3:        return vctrs_type2_double_s3;
    case vctrs_type_scalar:    return vctrs_type2_double_scalar;
    }
  }
  case vctrs_type_complex: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_complex;
    case vctrs_type_logical:   return vctrs_type2_logical_complex;
    case vctrs_type_integer:   return vctrs_type2_integer_complex;
    case vctrs_type_double:    return vctrs_type2_double_complex;
    case vctrs_type_complex:   return vctrs_type2_complex_complex;
    case vctrs_type_character: return vctrs_type2_complex_character;
    case vctrs_type_raw:       return vctrs_type2_complex_raw;
    case vctrs_type_list:      return vctrs_type2_complex_list;
    case vctrs_type_dataframe: return vctrs_type2_complex_dataframe;
    case vctrs_type_s3:        return vctrs_type2_complex_s3;
    case vctrs_type_scalar:    return vctrs_type2_complex_scalar;
    }
  }
  case vctrs_type_character: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_character;
    case vctrs_type_logical:   return vctrs_type2_logical_character;
    case vctrs_type_integer:   return vctrs_type2_integer_character;
    case vctrs_type_double:    return vctrs_type2_double_character;
    case vctrs_type_complex:   return vctrs_type2_complex_character;
    case vctrs_type_character: return vctrs_type2_character_character;
    case vctrs_type_raw:       return vctrs_type2_character_raw;
    case vctrs_type_list:      return vctrs_type2_character_list;
    case vctrs_type_dataframe: return vctrs_type2_character_dataframe;
    case vctrs_type_s3:        return vctrs_type2_character_s3;
    case vctrs_type_scalar:    return vctrs_type2_character_scalar;
    }
  }
  case vctrs_type_raw: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_raw;
    case vctrs_type_logical:   return vctrs_type2_logical_raw;
    case vctrs_type_integer:   return vctrs_type2_integer_raw;
    case vctrs_type_double:    return vctrs_type2_double_raw;
    case vctrs_type_complex:   return vctrs_type2_complex_raw;
    case vctrs_type_character: return vctrs_type2_character_raw;
    case vctrs_type_raw:       return vctrs_type2_raw_raw;
    case vctrs_type_list:      return vctrs_type2_raw_list;
    case vctrs_type_dataframe: return vctrs_type2_raw_dataframe;
    case vctrs_type_s3:        return vctrs_type2_raw_s3;
    case vctrs_type_scalar:    return vctrs_type2_raw_scalar;
    }
  }
  case vctrs_type_list: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_list;
    case vctrs_type_logical:   return vctrs_type2_logical_list;
    case vctrs_type_integer:   return vctrs_type2_integer_list;
    case vctrs_type_double:    return vctrs_type2_double_list;
    case vctrs_type_complex:   return vctrs_type2_complex_list;
    case vctrs_type_character: return vctrs_type2_character_list;
    case vctrs_type_raw:       return vctrs_type2_raw_list;
    case vctrs_type_list:      return vctrs_type2_list_list;
    case vctrs_type_dataframe: return vctrs_type2_list_dataframe;
    case vctrs_type_s3:        return vctrs_type2_list_s3;
    case vctrs_type_scalar:    return vctrs_type2_list_scalar;
    }
  }
  case vctrs_type_dataframe: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_dataframe;
    case vctrs_type_logical:   return vctrs_type2_logical_dataframe;
    case vctrs_type_integer:   return vctrs_type2_integer_dataframe;
    case vctrs_type_double:    return vctrs_type2_double_dataframe;
    case vctrs_type_complex:   return vctrs_type2_complex_dataframe;
    case vctrs_type_character: return vctrs_type2_character_dataframe;
    case vctrs_type_raw:       return vctrs_type2_raw_dataframe;
    case vctrs_type_list:      return vctrs_type2_list_dataframe;
    case vctrs_type_dataframe: return vctrs_type2_dataframe_dataframe;
    case vctrs_type_s3:        return vctrs_type2_dataframe_s3;
    case vctrs_type_scalar:    return vctrs_type2_dataframe_scalar;
    }
  }
  case vctrs_type_s3: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_s3;
    case vctrs_type_logical:   return vctrs_type2_logical_s3;
    case vctrs_type_integer:   return vctrs_type2_integer_s3;
    case vctrs_type_double:    return vctrs_type2_double_s3;
    case vctrs_type_complex:   return vctrs_type2_complex_s3;
    case vctrs_type_character: return vctrs_type2_character_s3;
    case vctrs_type_raw:       return vctrs_type2_raw_s3;
    case vctrs_type_list:      return vctrs_type2_list_s3;
    case vctrs_type_dataframe: return vctrs_type2_dataframe_s3;
    case vctrs_type_s3:        return vctrs_type2_s3_s3;
    case vctrs_type_scalar:    return vctrs_type2_s3_scalar;
    }
  }
  case vctrs_type_scalar: {
    switch (vec_typeof(y)) {
    case vctrs_type_null:      return vctrs_type2_null_scalar;
    case vctrs_type_logical:   return vctrs_type2_logical_scalar;
    case vctrs_type_integer:   return vctrs_type2_integer_scalar;
    case vctrs_type_double:    return vctrs_type2_double_scalar;
    case vctrs_type_complex:   return vctrs_type2_complex_scalar;
    case vctrs_type_character: return vctrs_type2_character_scalar;
    case vctrs_type_raw:       return vctrs_type2_raw_scalar;
    case vctrs_type_list:      return vctrs_type2_list_scalar;
    case vctrs_type_dataframe: return vctrs_type2_dataframe_scalar;
    case vctrs_type_s3:        return vctrs_type2_s3_scalar;
    case vctrs_type_scalar:    return vctrs_type2_scalar_scalar;
    }
  }}
};

const char* vctrs_type2_as_str(enum vctrs_type2 type) {
  switch (type) {
  case vctrs_type2_null_null:           return "vctrs_type2_null_null";
  case vctrs_type2_null_logical:        return "vctrs_type2_null_logical";
  case vctrs_type2_null_integer:        return "vctrs_type2_null_integer";
  case vctrs_type2_null_double:         return "vctrs_type2_null_double";
  case vctrs_type2_null_complex:        return "vctrs_type2_null_complex";
  case vctrs_type2_null_character:      return "vctrs_type2_null_character";
  case vctrs_type2_null_raw:            return "vctrs_type2_null_raw";
  case vctrs_type2_null_list:           return "vctrs_type2_null_list";
  case vctrs_type2_null_dataframe:      return "vctrs_type2_null_dataframe";
  case vctrs_type2_null_s3:             return "vctrs_type2_null_s3";
  case vctrs_type2_null_scalar:         return "vctrs_type2_null_scalar";

  case vctrs_type2_logical_logical:     return "vctrs_type2_logical_logical";
  case vctrs_type2_logical_integer:     return "vctrs_type2_logical_integer";
  case vctrs_type2_logical_double:      return "vctrs_type2_logical_double";
  case vctrs_type2_logical_complex:     return "vctrs_type2_logical_complex";
  case vctrs_type2_logical_character:   return "vctrs_type2_logical_character";
  case vctrs_type2_logical_raw:         return "vctrs_type2_logical_raw";
  case vctrs_type2_logical_list:        return "vctrs_type2_logical_list";
  case vctrs_type2_logical_dataframe:   return "vctrs_type2_logical_dataframe";
  case vctrs_type2_logical_s3:          return "vctrs_type2_logical_s3";
  case vctrs_type2_logical_scalar:      return "vctrs_type2_logical_scalar";

  case vctrs_type2_integer_integer:     return "vctrs_type2_integer_integer";
  case vctrs_type2_integer_double:      return "vctrs_type2_integer_double";
  case vctrs_type2_integer_complex:     return "vctrs_type2_integer_complex";
  case vctrs_type2_integer_character:   return "vctrs_type2_integer_character";
  case vctrs_type2_integer_raw:         return "vctrs_type2_integer_raw";
  case vctrs_type2_integer_list:        return "vctrs_type2_integer_list";
  case vctrs_type2_integer_dataframe:   return "vctrs_type2_integer_dataframe";
  case vctrs_type2_integer_s3:          return "vctrs_type2_integer_s3";
  case vctrs_type2_integer_scalar:      return "vctrs_type2_integer_scalar";

  case vctrs_type2_double_double:       return "vctrs_type2_double_double";
  case vctrs_type2_double_complex:      return "vctrs_type2_double_complex";
  case vctrs_type2_double_character:    return "vctrs_type2_double_character";
  case vctrs_type2_double_raw:          return "vctrs_type2_double_raw";
  case vctrs_type2_double_list:         return "vctrs_type2_double_list";
  case vctrs_type2_double_dataframe:    return "vctrs_type2_double_dataframe";
  case vctrs_type2_double_s3:           return "vctrs_type2_double_s3";
  case vctrs_type2_double_scalar:       return "vctrs_type2_double_scalar";

  case vctrs_type2_complex_complex:     return "vctrs_type2_complex_complex";
  case vctrs_type2_complex_character:   return "vctrs_type2_complex_character";
  case vctrs_type2_complex_raw:         return "vctrs_type2_complex_raw";
  case vctrs_type2_complex_list:        return "vctrs_type2_complex_list";
  case vctrs_type2_complex_dataframe:   return "vctrs_type2_complex_dataframe";
  case vctrs_type2_complex_s3:          return "vctrs_type2_complex_s3";
  case vctrs_type2_complex_scalar:      return "vctrs_type2_complex_scalar";

  case vctrs_type2_character_character: return "vctrs_type2_character_character";
  case vctrs_type2_character_raw:       return "vctrs_type2_character_raw";
  case vctrs_type2_character_list:      return "vctrs_type2_character_list";
  case vctrs_type2_character_dataframe: return "vctrs_type2_character_dataframe";
  case vctrs_type2_character_s3:        return "vctrs_type2_character_s3";
  case vctrs_type2_character_scalar:    return "vctrs_type2_character_scalar";

  case vctrs_type2_raw_raw:             return "vctrs_type2_raw_raw";
  case vctrs_type2_raw_list:            return "vctrs_type2_raw_list";
  case vctrs_type2_raw_dataframe:       return "vctrs_type2_raw_dataframe";
  case vctrs_type2_raw_s3:              return "vctrs_type2_raw_s3";
  case vctrs_type2_raw_scalar:          return "vctrs_type2_raw_scalar";

  case vctrs_type2_list_list:           return "vctrs_type2_list_list";
  case vctrs_type2_list_dataframe:      return "vctrs_type2_list_dataframe";
  case vctrs_type2_list_s3:             return "vctrs_type2_list_s3";
  case vctrs_type2_list_scalar:         return "vctrs_type2_list_scalar";

  case vctrs_type2_dataframe_dataframe: return "vctrs_type2_dataframe_dataframe";
  case vctrs_type2_dataframe_s3:        return "vctrs_type2_dataframe_s3";
  case vctrs_type2_dataframe_scalar:    return "vctrs_type2_dataframe_scalar";

  case vctrs_type2_s3_s3:               return "vctrs_type2_s3_s3";
  case vctrs_type2_s3_scalar:           return "vctrs_type2_s3_scalar";

  case vctrs_type2_scalar_scalar:       return "vctrs_type2_scalar_scalar";
  }
}

SEXP vctrs_typeof2(SEXP x, SEXP y) {
  enum vctrs_type2 type = vec_typeof2(x, y);
  return Rf_mkString(vctrs_type2_as_str(type));
}
