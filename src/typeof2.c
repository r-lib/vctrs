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

/**
 * [[ include("utils.h") ]]
 *
 * @param left Output parameter. Set to 1 when the common type comes
 *   from the left, 0 when it comes from the right, and -1 when it
 *   comes from both sides. This means that "left" is the default
 *   when coerced to a boolean value.
 */
enum vctrs_type2 vec_typeof2_impl(enum vctrs_type type_x,
                                  enum vctrs_type type_y,
                                  int* left) {
  switch (type_x) {
  case VCTRS_TYPE_null: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left = -1; return VCTRS_TYPE2_null_null;
    case VCTRS_TYPE_unspecified: *left =  0; return VCTRS_TYPE2_null_unspecified;
    case VCTRS_TYPE_logical:     *left =  0; return VCTRS_TYPE2_null_logical;
    case VCTRS_TYPE_integer:     *left =  0; return VCTRS_TYPE2_null_integer;
    case VCTRS_TYPE_double:      *left =  0; return VCTRS_TYPE2_null_double;
    case VCTRS_TYPE_complex:     *left =  0; return VCTRS_TYPE2_null_complex;
    case VCTRS_TYPE_character:   *left =  0; return VCTRS_TYPE2_null_character;
    case VCTRS_TYPE_raw:         *left =  0; return VCTRS_TYPE2_null_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_null_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_null_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_null_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_null_scalar;
    }
  }
  case VCTRS_TYPE_unspecified: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_unspecified;
    case VCTRS_TYPE_unspecified: *left = -1; return VCTRS_TYPE2_unspecified_unspecified;
    case VCTRS_TYPE_logical:     *left =  0; return VCTRS_TYPE2_unspecified_logical;
    case VCTRS_TYPE_integer:     *left =  0; return VCTRS_TYPE2_unspecified_integer;
    case VCTRS_TYPE_double:      *left =  0; return VCTRS_TYPE2_unspecified_double;
    case VCTRS_TYPE_complex:     *left =  0; return VCTRS_TYPE2_unspecified_complex;
    case VCTRS_TYPE_character:   *left =  0; return VCTRS_TYPE2_unspecified_character;
    case VCTRS_TYPE_raw:         *left =  0; return VCTRS_TYPE2_unspecified_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_unspecified_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_unspecified_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_unspecified_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_unspecified_scalar;
    }
  }
  case VCTRS_TYPE_logical: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_logical;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_logical;
    case VCTRS_TYPE_logical:     *left = -1; return VCTRS_TYPE2_logical_logical;
    case VCTRS_TYPE_integer:     *left =  0; return VCTRS_TYPE2_logical_integer;
    case VCTRS_TYPE_double:      *left =  0; return VCTRS_TYPE2_logical_double;
    case VCTRS_TYPE_complex:     *left =  0; return VCTRS_TYPE2_logical_complex;
    case VCTRS_TYPE_character:   *left =  0; return VCTRS_TYPE2_logical_character;
    case VCTRS_TYPE_raw:         *left =  0; return VCTRS_TYPE2_logical_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_logical_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_logical_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_logical_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_logical_scalar;
    }
  }
  case VCTRS_TYPE_integer: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_integer;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_integer;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_integer;
    case VCTRS_TYPE_integer:     *left = -1; return VCTRS_TYPE2_integer_integer;
    case VCTRS_TYPE_double:      *left =  0; return VCTRS_TYPE2_integer_double;
    case VCTRS_TYPE_complex:     *left =  0; return VCTRS_TYPE2_integer_complex;
    case VCTRS_TYPE_character:   *left =  0; return VCTRS_TYPE2_integer_character;
    case VCTRS_TYPE_raw:         *left =  0; return VCTRS_TYPE2_integer_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_integer_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_integer_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_integer_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_integer_scalar;
    }
  }
  case VCTRS_TYPE_double: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_double;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_double;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_double;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_double;
    case VCTRS_TYPE_double:      *left = -1; return VCTRS_TYPE2_double_double;
    case VCTRS_TYPE_complex:     *left =  0; return VCTRS_TYPE2_double_complex;
    case VCTRS_TYPE_character:   *left =  0; return VCTRS_TYPE2_double_character;
    case VCTRS_TYPE_raw:         *left =  0; return VCTRS_TYPE2_double_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_double_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_double_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_double_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_double_scalar;
    }
  }
  case VCTRS_TYPE_complex: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_complex;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_complex;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_complex;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_complex;
    case VCTRS_TYPE_double:      *left =  1; return VCTRS_TYPE2_double_complex;
    case VCTRS_TYPE_complex:     *left = -1; return VCTRS_TYPE2_complex_complex;
    case VCTRS_TYPE_character:   *left =  0; return VCTRS_TYPE2_complex_character;
    case VCTRS_TYPE_raw:         *left =  0; return VCTRS_TYPE2_complex_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_complex_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_complex_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_complex_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_complex_scalar;
    }
  }
  case VCTRS_TYPE_character: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_character;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_character;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_character;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_character;
    case VCTRS_TYPE_double:      *left =  1; return VCTRS_TYPE2_double_character;
    case VCTRS_TYPE_complex:     *left =  1; return VCTRS_TYPE2_complex_character;
    case VCTRS_TYPE_character:   *left = -1; return VCTRS_TYPE2_character_character;
    case VCTRS_TYPE_raw:         *left =  0; return VCTRS_TYPE2_character_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_character_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_character_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_character_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_character_scalar;
    }
  }
  case VCTRS_TYPE_raw: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_raw;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_raw;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_raw;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_raw;
    case VCTRS_TYPE_double:      *left =  1; return VCTRS_TYPE2_double_raw;
    case VCTRS_TYPE_complex:     *left =  1; return VCTRS_TYPE2_complex_raw;
    case VCTRS_TYPE_character:   *left =  1; return VCTRS_TYPE2_character_raw;
    case VCTRS_TYPE_raw:         *left = -1; return VCTRS_TYPE2_raw_raw;
    case VCTRS_TYPE_list:        *left =  0; return VCTRS_TYPE2_raw_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_raw_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_raw_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_raw_scalar;
    }
  }
  case VCTRS_TYPE_list: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_list;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_list;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_list;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_list;
    case VCTRS_TYPE_double:      *left =  1; return VCTRS_TYPE2_double_list;
    case VCTRS_TYPE_complex:     *left =  1; return VCTRS_TYPE2_complex_list;
    case VCTRS_TYPE_character:   *left =  1; return VCTRS_TYPE2_character_list;
    case VCTRS_TYPE_raw:         *left =  1; return VCTRS_TYPE2_raw_list;
    case VCTRS_TYPE_list:        *left = -1; return VCTRS_TYPE2_list_list;
    case VCTRS_TYPE_dataframe:   *left =  0; return VCTRS_TYPE2_list_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_list_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_list_scalar;
    }
  }
  case VCTRS_TYPE_dataframe: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_dataframe;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_dataframe;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_dataframe;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_dataframe;
    case VCTRS_TYPE_double:      *left =  1; return VCTRS_TYPE2_double_dataframe;
    case VCTRS_TYPE_complex:     *left =  1; return VCTRS_TYPE2_complex_dataframe;
    case VCTRS_TYPE_character:   *left =  1; return VCTRS_TYPE2_character_dataframe;
    case VCTRS_TYPE_raw:         *left =  1; return VCTRS_TYPE2_raw_dataframe;
    case VCTRS_TYPE_list:        *left =  1; return VCTRS_TYPE2_list_dataframe;
    case VCTRS_TYPE_dataframe:   *left = -1; return VCTRS_TYPE2_dataframe_dataframe;
    case VCTRS_TYPE_s3:          *left =  0; return VCTRS_TYPE2_dataframe_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_dataframe_scalar;
    }
  }
  case VCTRS_TYPE_s3: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_s3;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_s3;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_s3;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_s3;
    case VCTRS_TYPE_double:      *left =  1; return VCTRS_TYPE2_double_s3;
    case VCTRS_TYPE_complex:     *left =  1; return VCTRS_TYPE2_complex_s3;
    case VCTRS_TYPE_character:   *left =  1; return VCTRS_TYPE2_character_s3;
    case VCTRS_TYPE_raw:         *left =  1; return VCTRS_TYPE2_raw_s3;
    case VCTRS_TYPE_list:        *left =  1; return VCTRS_TYPE2_list_s3;
    case VCTRS_TYPE_dataframe:   *left =  1; return VCTRS_TYPE2_dataframe_s3;
    case VCTRS_TYPE_s3:          *left = -1; return VCTRS_TYPE2_S3_s3;
    case VCTRS_TYPE_scalar:      *left =  0; return VCTRS_TYPE2_S3_scalar;
    }
  }
  case VCTRS_TYPE_scalar: {
    switch (type_y) {
    case VCTRS_TYPE_null:        *left =  1; return VCTRS_TYPE2_null_scalar;
    case VCTRS_TYPE_unspecified: *left =  1; return VCTRS_TYPE2_unspecified_scalar;
    case VCTRS_TYPE_logical:     *left =  1; return VCTRS_TYPE2_logical_scalar;
    case VCTRS_TYPE_integer:     *left =  1; return VCTRS_TYPE2_integer_scalar;
    case VCTRS_TYPE_double:      *left =  1; return VCTRS_TYPE2_double_scalar;
    case VCTRS_TYPE_complex:     *left =  1; return VCTRS_TYPE2_complex_scalar;
    case VCTRS_TYPE_character:   *left =  1; return VCTRS_TYPE2_character_scalar;
    case VCTRS_TYPE_raw:         *left =  1; return VCTRS_TYPE2_raw_scalar;
    case VCTRS_TYPE_list:        *left =  1; return VCTRS_TYPE2_list_scalar;
    case VCTRS_TYPE_dataframe:   *left =  1; return VCTRS_TYPE2_dataframe_scalar;
    case VCTRS_TYPE_s3:          *left =  1; return VCTRS_TYPE2_S3_scalar;
    case VCTRS_TYPE_scalar:      *left = -1; return VCTRS_TYPE2_scalar_scalar;
    }
  }}

  r_stop_unreachable();
}

enum vctrs_type2 vec_typeof2(r_obj* x, r_obj* y) {
  int _;
  return vec_typeof2_impl(vec_typeof(x), vec_typeof(y), &_);
}

const char* vctrs_type2_as_str(enum vctrs_type2 type) {
  switch (type) {
  case VCTRS_TYPE2_null_null:               return "VCTRS_TYPE2_null_null";
  case VCTRS_TYPE2_null_logical:            return "VCTRS_TYPE2_null_logical";
  case VCTRS_TYPE2_null_integer:            return "VCTRS_TYPE2_null_integer";
  case VCTRS_TYPE2_null_double:             return "VCTRS_TYPE2_null_double";
  case VCTRS_TYPE2_null_complex:            return "VCTRS_TYPE2_null_complex";
  case VCTRS_TYPE2_null_character:          return "VCTRS_TYPE2_null_character";
  case VCTRS_TYPE2_null_raw:                return "VCTRS_TYPE2_null_raw";
  case VCTRS_TYPE2_null_list:               return "VCTRS_TYPE2_null_list";
  case VCTRS_TYPE2_null_dataframe:          return "VCTRS_TYPE2_null_dataframe";
  case VCTRS_TYPE2_null_s3:                 return "VCTRS_TYPE2_null_s3";
  case VCTRS_TYPE2_null_unspecified:        return "VCTRS_TYPE2_null_unspecified";
  case VCTRS_TYPE2_null_scalar:             return "VCTRS_TYPE2_null_scalar";

  case VCTRS_TYPE2_unspecified_logical:     return "VCTRS_TYPE2_unspecified_logical";
  case VCTRS_TYPE2_unspecified_integer:     return "VCTRS_TYPE2_unspecified_integer";
  case VCTRS_TYPE2_unspecified_double:      return "VCTRS_TYPE2_unspecified_double";
  case VCTRS_TYPE2_unspecified_complex:     return "VCTRS_TYPE2_unspecified_complex";
  case VCTRS_TYPE2_unspecified_character:   return "VCTRS_TYPE2_unspecified_character";
  case VCTRS_TYPE2_unspecified_raw:         return "VCTRS_TYPE2_unspecified_raw";
  case VCTRS_TYPE2_unspecified_list:        return "VCTRS_TYPE2_unspecified_list";
  case VCTRS_TYPE2_unspecified_dataframe:   return "VCTRS_TYPE2_unspecified_dataframe";
  case VCTRS_TYPE2_unspecified_s3:          return "VCTRS_TYPE2_unspecified_s3";
  case VCTRS_TYPE2_unspecified_unspecified: return "VCTRS_TYPE2_unspecified_unspecified";
  case VCTRS_TYPE2_unspecified_scalar:      return "VCTRS_TYPE2_unspecified_scalar";

  case VCTRS_TYPE2_logical_logical:         return "VCTRS_TYPE2_logical_logical";
  case VCTRS_TYPE2_logical_integer:         return "VCTRS_TYPE2_logical_integer";
  case VCTRS_TYPE2_logical_double:          return "VCTRS_TYPE2_logical_double";
  case VCTRS_TYPE2_logical_complex:         return "VCTRS_TYPE2_logical_complex";
  case VCTRS_TYPE2_logical_character:       return "VCTRS_TYPE2_logical_character";
  case VCTRS_TYPE2_logical_raw:             return "VCTRS_TYPE2_logical_raw";
  case VCTRS_TYPE2_logical_list:            return "VCTRS_TYPE2_logical_list";
  case VCTRS_TYPE2_logical_dataframe:       return "VCTRS_TYPE2_logical_dataframe";
  case VCTRS_TYPE2_logical_s3:              return "VCTRS_TYPE2_logical_s3";
  case VCTRS_TYPE2_logical_scalar:          return "VCTRS_TYPE2_logical_scalar";

  case VCTRS_TYPE2_integer_integer:         return "VCTRS_TYPE2_integer_integer";
  case VCTRS_TYPE2_integer_double:          return "VCTRS_TYPE2_integer_double";
  case VCTRS_TYPE2_integer_complex:         return "VCTRS_TYPE2_integer_complex";
  case VCTRS_TYPE2_integer_character:       return "VCTRS_TYPE2_integer_character";
  case VCTRS_TYPE2_integer_raw:             return "VCTRS_TYPE2_integer_raw";
  case VCTRS_TYPE2_integer_list:            return "VCTRS_TYPE2_integer_list";
  case VCTRS_TYPE2_integer_dataframe:       return "VCTRS_TYPE2_integer_dataframe";
  case VCTRS_TYPE2_integer_s3:              return "VCTRS_TYPE2_integer_s3";
  case VCTRS_TYPE2_integer_scalar:          return "VCTRS_TYPE2_integer_scalar";

  case VCTRS_TYPE2_double_double:           return "VCTRS_TYPE2_double_double";
  case VCTRS_TYPE2_double_complex:          return "VCTRS_TYPE2_double_complex";
  case VCTRS_TYPE2_double_character:        return "VCTRS_TYPE2_double_character";
  case VCTRS_TYPE2_double_raw:              return "VCTRS_TYPE2_double_raw";
  case VCTRS_TYPE2_double_list:             return "VCTRS_TYPE2_double_list";
  case VCTRS_TYPE2_double_dataframe:        return "VCTRS_TYPE2_double_dataframe";
  case VCTRS_TYPE2_double_s3:               return "VCTRS_TYPE2_double_s3";
  case VCTRS_TYPE2_double_scalar:           return "VCTRS_TYPE2_double_scalar";

  case VCTRS_TYPE2_complex_complex:         return "VCTRS_TYPE2_complex_complex";
  case VCTRS_TYPE2_complex_character:       return "VCTRS_TYPE2_complex_character";
  case VCTRS_TYPE2_complex_raw:             return "VCTRS_TYPE2_complex_raw";
  case VCTRS_TYPE2_complex_list:            return "VCTRS_TYPE2_complex_list";
  case VCTRS_TYPE2_complex_dataframe:       return "VCTRS_TYPE2_complex_dataframe";
  case VCTRS_TYPE2_complex_s3:              return "VCTRS_TYPE2_complex_s3";
  case VCTRS_TYPE2_complex_scalar:          return "VCTRS_TYPE2_complex_scalar";

  case VCTRS_TYPE2_character_character:     return "VCTRS_TYPE2_character_character";
  case VCTRS_TYPE2_character_raw:           return "VCTRS_TYPE2_character_raw";
  case VCTRS_TYPE2_character_list:          return "VCTRS_TYPE2_character_list";
  case VCTRS_TYPE2_character_dataframe:     return "VCTRS_TYPE2_character_dataframe";
  case VCTRS_TYPE2_character_s3:            return "VCTRS_TYPE2_character_s3";
  case VCTRS_TYPE2_character_scalar:        return "VCTRS_TYPE2_character_scalar";

  case VCTRS_TYPE2_raw_raw:                 return "VCTRS_TYPE2_raw_raw";
  case VCTRS_TYPE2_raw_list:                return "VCTRS_TYPE2_raw_list";
  case VCTRS_TYPE2_raw_dataframe:           return "VCTRS_TYPE2_raw_dataframe";
  case VCTRS_TYPE2_raw_s3:                  return "VCTRS_TYPE2_raw_s3";
  case VCTRS_TYPE2_raw_scalar:              return "VCTRS_TYPE2_raw_scalar";

  case VCTRS_TYPE2_list_list:               return "VCTRS_TYPE2_list_list";
  case VCTRS_TYPE2_list_dataframe:          return "VCTRS_TYPE2_list_dataframe";
  case VCTRS_TYPE2_list_s3:                 return "VCTRS_TYPE2_list_s3";
  case VCTRS_TYPE2_list_scalar:             return "VCTRS_TYPE2_list_scalar";

  case VCTRS_TYPE2_dataframe_dataframe:     return "VCTRS_TYPE2_dataframe_dataframe";
  case VCTRS_TYPE2_dataframe_s3:            return "VCTRS_TYPE2_dataframe_s3";
  case VCTRS_TYPE2_dataframe_scalar:        return "VCTRS_TYPE2_dataframe_scalar";

  case VCTRS_TYPE2_S3_s3:                   return "VCTRS_TYPE2_S3_s3";
  case VCTRS_TYPE2_S3_scalar:               return "VCTRS_TYPE2_S3_scalar";

  case VCTRS_TYPE2_scalar_scalar:           return "VCTRS_TYPE2_scalar_scalar";
  }

  r_stop_unreachable();
}

r_obj* ffi_typeof2(r_obj* x, r_obj* y) {
  enum vctrs_type2 type = vec_typeof2(x, y);
  return r_chr(vctrs_type2_as_str(type));
}
