#include "vctrs.h"
#include "decl/typeof2-s3-decl.h"

enum vctrs_type2_s3 vec_typeof2_s3_impl(r_obj* x,
                                        r_obj* y,
                                        enum vctrs_type type_x,
                                        enum vctrs_type type_y,
                                        int* left) {
  switch (type_x) {
  case VCTRS_TYPE_null: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_null_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_null_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_null_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_null_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_null_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_null_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_null_unknown;
    }
  }
  case VCTRS_TYPE_unspecified: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_unspecified_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_unspecified_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_unspecified_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_unspecified_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_unspecified_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_unspecified_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_unspecified_unknown;
    }
  }
  case VCTRS_TYPE_logical: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_logical_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_logical_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_logical_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_logical_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_logical_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_logical_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_logical_unknown;
    }
  }
  case VCTRS_TYPE_integer: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_integer_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_integer_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_integer_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_integer_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_integer_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_integer_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_integer_unknown;
    }
  }
  case VCTRS_TYPE_double: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_double_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_double_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_double_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_double_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_double_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_double_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_double_unknown;
    }
  }
  case VCTRS_TYPE_complex: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_complex_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_complex_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_complex_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_complex_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_complex_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_complex_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_complex_unknown;
    }
  }
  case VCTRS_TYPE_character: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_character_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_character_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_character_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_character_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_character_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_character_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_character_unknown;
    }
  }
  case VCTRS_TYPE_raw: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_raw_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_raw_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_raw_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_raw_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_raw_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_raw_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_raw_unknown;
    }
  }
  case VCTRS_TYPE_list: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_list_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_list_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_list_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_list_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_list_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_list_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_list_unknown;
    }
  }
  case VCTRS_TYPE_dataframe: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_dataframe_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_dataframe_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_dataframe_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_dataframe_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_dataframe_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_dataframe_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_dataframe_unknown;
    }
  }
  case VCTRS_TYPE_scalar: {
    switch (class_type(y)) {
    case VCTRS_CLASS_bare_factor:  *left = 0; return VCTRS_TYPE2_S3_scalar_bare_factor;
    case VCTRS_CLASS_bare_ordered: *left = 0; return VCTRS_TYPE2_S3_scalar_bare_ordered;
    case VCTRS_CLASS_bare_date:    *left = 0; return VCTRS_TYPE2_S3_scalar_bare_date;
    case VCTRS_CLASS_bare_posixct: *left = 0; return VCTRS_TYPE2_S3_scalar_bare_posixct;
    case VCTRS_CLASS_bare_posixlt: *left = 0; return VCTRS_TYPE2_S3_scalar_bare_posixlt;
    case VCTRS_CLASS_bare_tibble:  *left = 0; return VCTRS_TYPE2_S3_scalar_bare_tibble;
    default:                       *left = 0; return VCTRS_TYPE2_S3_scalar_unknown;
    }
  }
  case VCTRS_TYPE_s3: {
    return vec_typeof2_s3_impl2(x, y, type_y, left);
  }}

  r_stop_unreachable();
}


static
enum vctrs_type2_s3 vec_typeof2_s3_impl2(r_obj* x,
                                         r_obj* y,
                                         enum vctrs_type type_y,
                                         int* left) {
  switch (class_type(x)) {
  case VCTRS_CLASS_bare_factor: {
    switch (type_y) {
    case VCTRS_TYPE_null:            *left =  1; return VCTRS_TYPE2_S3_null_bare_factor;
    case VCTRS_TYPE_unspecified:     *left =  1; return VCTRS_TYPE2_S3_unspecified_bare_factor;
    case VCTRS_TYPE_logical:         *left =  1; return VCTRS_TYPE2_S3_logical_bare_factor;
    case VCTRS_TYPE_integer:         *left =  1; return VCTRS_TYPE2_S3_integer_bare_factor;
    case VCTRS_TYPE_double:          *left =  1; return VCTRS_TYPE2_S3_double_bare_factor;
    case VCTRS_TYPE_complex:         *left =  1; return VCTRS_TYPE2_S3_complex_bare_factor;
    case VCTRS_TYPE_character:       *left =  1; return VCTRS_TYPE2_S3_character_bare_factor;
    case VCTRS_TYPE_raw:             *left =  1; return VCTRS_TYPE2_S3_raw_bare_factor;
    case VCTRS_TYPE_list:            *left =  1; return VCTRS_TYPE2_S3_list_bare_factor;
    case VCTRS_TYPE_dataframe:       *left =  1; return VCTRS_TYPE2_S3_dataframe_bare_factor;
    case VCTRS_TYPE_scalar:          *left =  1; return VCTRS_TYPE2_S3_scalar_bare_factor;
    case VCTRS_TYPE_s3: {
      switch (class_type(y)) {
      case VCTRS_CLASS_bare_factor:  *left = -1; return VCTRS_TYPE2_S3_bare_factor_bare_factor;
      case VCTRS_CLASS_bare_ordered: *left =  0; return VCTRS_TYPE2_S3_bare_factor_bare_ordered;
      case VCTRS_CLASS_bare_date:    *left =  0; return VCTRS_TYPE2_S3_bare_factor_bare_date;
      case VCTRS_CLASS_bare_posixct: *left =  0; return VCTRS_TYPE2_S3_bare_factor_bare_posixct;
      case VCTRS_CLASS_bare_posixlt: *left =  0; return VCTRS_TYPE2_S3_bare_factor_bare_posixlt;
      case VCTRS_CLASS_bare_tibble:  *left =  0; return VCTRS_TYPE2_S3_bare_factor_bare_tibble;
      default:                       *left =  0; return VCTRS_TYPE2_S3_bare_factor_unknown;
      }
    }}
  }
  case VCTRS_CLASS_bare_ordered: {
    switch (type_y) {
    case VCTRS_TYPE_null:            *left =  1; return VCTRS_TYPE2_S3_null_bare_ordered;
    case VCTRS_TYPE_unspecified:     *left =  1; return VCTRS_TYPE2_S3_unspecified_bare_ordered;
    case VCTRS_TYPE_logical:         *left =  1; return VCTRS_TYPE2_S3_logical_bare_ordered;
    case VCTRS_TYPE_integer:         *left =  1; return VCTRS_TYPE2_S3_integer_bare_ordered;
    case VCTRS_TYPE_double:          *left =  1; return VCTRS_TYPE2_S3_double_bare_ordered;
    case VCTRS_TYPE_complex:         *left =  1; return VCTRS_TYPE2_S3_complex_bare_ordered;
    case VCTRS_TYPE_character:       *left =  1; return VCTRS_TYPE2_S3_character_bare_ordered;
    case VCTRS_TYPE_raw:             *left =  1; return VCTRS_TYPE2_S3_raw_bare_ordered;
    case VCTRS_TYPE_list:            *left =  1; return VCTRS_TYPE2_S3_list_bare_ordered;
    case VCTRS_TYPE_dataframe:       *left =  1; return VCTRS_TYPE2_S3_dataframe_bare_ordered;
    case VCTRS_TYPE_scalar:          *left =  1; return VCTRS_TYPE2_S3_scalar_bare_ordered;
    case VCTRS_TYPE_s3: {
      switch (class_type(y)) {
      case VCTRS_CLASS_bare_factor:  *left =  1; return VCTRS_TYPE2_S3_bare_factor_bare_ordered;
      case VCTRS_CLASS_bare_ordered: *left = -1; return VCTRS_TYPE2_S3_bare_ordered_bare_ordered;
      case VCTRS_CLASS_bare_date:    *left =  0; return VCTRS_TYPE2_S3_bare_ordered_bare_date;
      case VCTRS_CLASS_bare_posixct: *left =  0; return VCTRS_TYPE2_S3_bare_ordered_bare_posixct;
      case VCTRS_CLASS_bare_posixlt: *left =  0; return VCTRS_TYPE2_S3_bare_ordered_bare_posixlt;
      case VCTRS_CLASS_bare_tibble:  *left =  0; return VCTRS_TYPE2_S3_bare_ordered_bare_tibble;
      default:                       *left =  0; return VCTRS_TYPE2_S3_bare_ordered_unknown;
      }
    }}
  }
  case VCTRS_CLASS_bare_date: {
    switch (type_y) {
    case VCTRS_TYPE_null:            *left =  1; return VCTRS_TYPE2_S3_null_bare_date;
    case VCTRS_TYPE_unspecified:     *left =  1; return VCTRS_TYPE2_S3_unspecified_bare_date;
    case VCTRS_TYPE_logical:         *left =  1; return VCTRS_TYPE2_S3_logical_bare_date;
    case VCTRS_TYPE_integer:         *left =  1; return VCTRS_TYPE2_S3_integer_bare_date;
    case VCTRS_TYPE_double:          *left =  1; return VCTRS_TYPE2_S3_double_bare_date;
    case VCTRS_TYPE_complex:         *left =  1; return VCTRS_TYPE2_S3_complex_bare_date;
    case VCTRS_TYPE_character:       *left =  1; return VCTRS_TYPE2_S3_character_bare_date;
    case VCTRS_TYPE_raw:             *left =  1; return VCTRS_TYPE2_S3_raw_bare_date;
    case VCTRS_TYPE_list:            *left =  1; return VCTRS_TYPE2_S3_list_bare_date;
    case VCTRS_TYPE_dataframe:       *left =  1; return VCTRS_TYPE2_S3_dataframe_bare_date;
    case VCTRS_TYPE_scalar:          *left =  1; return VCTRS_TYPE2_S3_scalar_bare_date;
    case VCTRS_TYPE_s3: {
      switch (class_type(y)) {
      case VCTRS_CLASS_bare_factor:  *left =  1; return VCTRS_TYPE2_S3_bare_factor_bare_date;
      case VCTRS_CLASS_bare_ordered: *left =  1; return VCTRS_TYPE2_S3_bare_ordered_bare_date;
      case VCTRS_CLASS_bare_date:    *left = -1; return VCTRS_TYPE2_S3_bare_date_bare_date;
      case VCTRS_CLASS_bare_posixct: *left =  0; return VCTRS_TYPE2_S3_bare_date_bare_posixct;
      case VCTRS_CLASS_bare_posixlt: *left =  0; return VCTRS_TYPE2_S3_bare_date_bare_posixlt;
      case VCTRS_CLASS_bare_tibble:  *left =  0; return VCTRS_TYPE2_S3_bare_date_bare_tibble;
      default:                       *left =  0; return VCTRS_TYPE2_S3_bare_date_unknown;
      }
    }}
  }
  case VCTRS_CLASS_bare_posixct: {
    switch (type_y) {
    case VCTRS_TYPE_null:            *left =  1; return VCTRS_TYPE2_S3_null_bare_posixct;
    case VCTRS_TYPE_unspecified:     *left =  1; return VCTRS_TYPE2_S3_unspecified_bare_posixct;
    case VCTRS_TYPE_logical:         *left =  1; return VCTRS_TYPE2_S3_logical_bare_posixct;
    case VCTRS_TYPE_integer:         *left =  1; return VCTRS_TYPE2_S3_integer_bare_posixct;
    case VCTRS_TYPE_double:          *left =  1; return VCTRS_TYPE2_S3_double_bare_posixct;
    case VCTRS_TYPE_complex:         *left =  1; return VCTRS_TYPE2_S3_complex_bare_posixct;
    case VCTRS_TYPE_character:       *left =  1; return VCTRS_TYPE2_S3_character_bare_posixct;
    case VCTRS_TYPE_raw:             *left =  1; return VCTRS_TYPE2_S3_raw_bare_posixct;
    case VCTRS_TYPE_list:            *left =  1; return VCTRS_TYPE2_S3_list_bare_posixct;
    case VCTRS_TYPE_dataframe:       *left =  1; return VCTRS_TYPE2_S3_dataframe_bare_posixct;
    case VCTRS_TYPE_scalar:          *left =  1; return VCTRS_TYPE2_S3_scalar_bare_posixct;
    case VCTRS_TYPE_s3: {
      switch (class_type(y)) {
      case VCTRS_CLASS_bare_factor:  *left =  1; return VCTRS_TYPE2_S3_bare_factor_bare_posixct;
      case VCTRS_CLASS_bare_ordered: *left =  1; return VCTRS_TYPE2_S3_bare_ordered_bare_posixct;
      case VCTRS_CLASS_bare_date:    *left =  1; return VCTRS_TYPE2_S3_bare_date_bare_posixct;
      case VCTRS_CLASS_bare_posixct: *left = -1; return VCTRS_TYPE2_S3_bare_posixct_bare_posixct;
      case VCTRS_CLASS_bare_posixlt: *left =  0; return VCTRS_TYPE2_S3_bare_posixct_bare_posixlt;
      case VCTRS_CLASS_bare_tibble:  *left =  0; return VCTRS_TYPE2_S3_bare_posixct_bare_tibble;
      default:                       *left =  0; return VCTRS_TYPE2_S3_bare_posixct_unknown;
      }
    }}
  }
  case VCTRS_CLASS_bare_posixlt: {
    switch (type_y) {
    case VCTRS_TYPE_null:            *left =  1; return VCTRS_TYPE2_S3_null_bare_posixlt;
    case VCTRS_TYPE_unspecified:     *left =  1; return VCTRS_TYPE2_S3_unspecified_bare_posixlt;
    case VCTRS_TYPE_logical:         *left =  1; return VCTRS_TYPE2_S3_logical_bare_posixlt;
    case VCTRS_TYPE_integer:         *left =  1; return VCTRS_TYPE2_S3_integer_bare_posixlt;
    case VCTRS_TYPE_double:          *left =  1; return VCTRS_TYPE2_S3_double_bare_posixlt;
    case VCTRS_TYPE_complex:         *left =  1; return VCTRS_TYPE2_S3_complex_bare_posixlt;
    case VCTRS_TYPE_character:       *left =  1; return VCTRS_TYPE2_S3_character_bare_posixlt;
    case VCTRS_TYPE_raw:             *left =  1; return VCTRS_TYPE2_S3_raw_bare_posixlt;
    case VCTRS_TYPE_list:            *left =  1; return VCTRS_TYPE2_S3_list_bare_posixlt;
    case VCTRS_TYPE_dataframe:       *left =  1; return VCTRS_TYPE2_S3_dataframe_bare_posixlt;
    case VCTRS_TYPE_scalar:          *left =  1; return VCTRS_TYPE2_S3_scalar_bare_posixlt;
    case VCTRS_TYPE_s3: {
      switch (class_type(y)) {
      case VCTRS_CLASS_bare_factor:  *left =  1; return VCTRS_TYPE2_S3_bare_factor_bare_posixlt;
      case VCTRS_CLASS_bare_ordered: *left =  1; return VCTRS_TYPE2_S3_bare_ordered_bare_posixlt;
      case VCTRS_CLASS_bare_date:    *left =  1; return VCTRS_TYPE2_S3_bare_date_bare_posixlt;
      case VCTRS_CLASS_bare_posixct: *left =  1; return VCTRS_TYPE2_S3_bare_posixct_bare_posixlt;
      case VCTRS_CLASS_bare_posixlt: *left = -1; return VCTRS_TYPE2_S3_bare_posixlt_bare_posixlt;
      case VCTRS_CLASS_bare_tibble:  *left =  0; return VCTRS_TYPE2_S3_bare_posixlt_bare_tibble;
      default:                       *left =  0; return VCTRS_TYPE2_S3_bare_posixlt_unknown;
      }
    }}
  }
  case VCTRS_CLASS_bare_tibble: {
    switch (type_y) {
    case VCTRS_TYPE_null:            *left =  1; return VCTRS_TYPE2_S3_null_bare_tibble;
    case VCTRS_TYPE_unspecified:     *left =  1; return VCTRS_TYPE2_S3_unspecified_bare_tibble;
    case VCTRS_TYPE_logical:         *left =  1; return VCTRS_TYPE2_S3_logical_bare_tibble;
    case VCTRS_TYPE_integer:         *left =  1; return VCTRS_TYPE2_S3_integer_bare_tibble;
    case VCTRS_TYPE_double:          *left =  1; return VCTRS_TYPE2_S3_double_bare_tibble;
    case VCTRS_TYPE_complex:         *left =  1; return VCTRS_TYPE2_S3_complex_bare_tibble;
    case VCTRS_TYPE_character:       *left =  1; return VCTRS_TYPE2_S3_character_bare_tibble;
    case VCTRS_TYPE_raw:             *left =  1; return VCTRS_TYPE2_S3_raw_bare_tibble;
    case VCTRS_TYPE_list:            *left =  1; return VCTRS_TYPE2_S3_list_bare_tibble;
    case VCTRS_TYPE_dataframe:       *left =  1; return VCTRS_TYPE2_S3_dataframe_bare_tibble;
    case VCTRS_TYPE_scalar:          *left =  1; return VCTRS_TYPE2_S3_scalar_bare_tibble;
    case VCTRS_TYPE_s3: {
      switch (class_type(y)) {
      case VCTRS_CLASS_bare_factor:  *left =  1; return VCTRS_TYPE2_S3_bare_factor_bare_tibble;
      case VCTRS_CLASS_bare_ordered: *left =  1; return VCTRS_TYPE2_S3_bare_ordered_bare_tibble;
      case VCTRS_CLASS_bare_date:    *left =  1; return VCTRS_TYPE2_S3_bare_date_bare_tibble;
      case VCTRS_CLASS_bare_posixct: *left =  1; return VCTRS_TYPE2_S3_bare_posixct_bare_tibble;
      case VCTRS_CLASS_bare_posixlt: *left =  1; return VCTRS_TYPE2_S3_bare_posixlt_bare_tibble;
      case VCTRS_CLASS_bare_tibble:  *left = -1; return VCTRS_TYPE2_S3_bare_tibble_bare_tibble;
      default:                       *left =  0; return VCTRS_TYPE2_S3_bare_tibble_unknown;
      }
    }}
  }
  default: {
    switch (type_y) {
    case VCTRS_TYPE_null:            *left =  1; return VCTRS_TYPE2_S3_null_unknown;
    case VCTRS_TYPE_unspecified:     *left =  1; return VCTRS_TYPE2_S3_unspecified_unknown;
    case VCTRS_TYPE_logical:         *left =  1; return VCTRS_TYPE2_S3_logical_unknown;
    case VCTRS_TYPE_integer:         *left =  1; return VCTRS_TYPE2_S3_integer_unknown;
    case VCTRS_TYPE_double:          *left =  1; return VCTRS_TYPE2_S3_double_unknown;
    case VCTRS_TYPE_complex:         *left =  1; return VCTRS_TYPE2_S3_complex_unknown;
    case VCTRS_TYPE_character:       *left =  1; return VCTRS_TYPE2_S3_character_unknown;
    case VCTRS_TYPE_raw:             *left =  1; return VCTRS_TYPE2_S3_raw_unknown;
    case VCTRS_TYPE_list:            *left =  1; return VCTRS_TYPE2_S3_list_unknown;
    case VCTRS_TYPE_dataframe:       *left =  1; return VCTRS_TYPE2_S3_dataframe_unknown;
    case VCTRS_TYPE_scalar:          *left =  1; return VCTRS_TYPE2_S3_scalar_unknown;
    case VCTRS_TYPE_s3: {
      switch (class_type(y)) {
      case VCTRS_CLASS_bare_factor:  *left =  1; return VCTRS_TYPE2_S3_bare_factor_unknown;
      case VCTRS_CLASS_bare_ordered: *left =  1; return VCTRS_TYPE2_S3_bare_ordered_unknown;
      case VCTRS_CLASS_bare_date:    *left =  1; return VCTRS_TYPE2_S3_bare_date_unknown;
      case VCTRS_CLASS_bare_posixct: *left =  1; return VCTRS_TYPE2_S3_bare_posixct_unknown;
      case VCTRS_CLASS_bare_posixlt: *left =  1; return VCTRS_TYPE2_S3_bare_posixlt_unknown;
      case VCTRS_CLASS_bare_tibble:  *left =  1; return VCTRS_TYPE2_S3_bare_tibble_unknown;
      default:                       *left = -1; return VCTRS_TYPE2_S3_unknown_unknown;
      }
    }}
  }}

  r_stop_unreachable();
}

static
enum vctrs_type2_s3 vec_typeof2_s3(r_obj* x, r_obj* y) {
  int _;
  return vec_typeof2_s3_impl(x, y, vec_typeof(x), vec_typeof(y), &_);
}

static
const char* vctrs_type2_s3_as_str(enum vctrs_type2_s3 type) {
  switch (type) {
  case VCTRS_TYPE2_S3_null_bare_factor:            return "VCTRS_TYPE2_S3_null_bare_factor";
  case VCTRS_TYPE2_S3_null_bare_ordered:           return "VCTRS_TYPE2_S3_null_bare_ordered";
  case VCTRS_TYPE2_S3_null_bare_date:              return "VCTRS_TYPE2_S3_null_bare_date";
  case VCTRS_TYPE2_S3_null_bare_posixct:           return "VCTRS_TYPE2_S3_null_bare_posixct";
  case VCTRS_TYPE2_S3_null_bare_posixlt:           return "VCTRS_TYPE2_S3_null_bare_posixlt";
  case VCTRS_TYPE2_S3_null_bare_tibble:            return "VCTRS_TYPE2_S3_null_bare_tibble";
  case VCTRS_TYPE2_S3_null_unknown:                return "VCTRS_TYPE2_S3_null_unknown";

  case VCTRS_TYPE2_S3_unspecified_bare_factor:     return "VCTRS_TYPE2_S3_unspecified_bare_factor";
  case VCTRS_TYPE2_S3_unspecified_bare_ordered:    return "VCTRS_TYPE2_S3_unspecified_bare_ordered";
  case VCTRS_TYPE2_S3_unspecified_bare_date:       return "VCTRS_TYPE2_S3_unspecified_bare_date";
  case VCTRS_TYPE2_S3_unspecified_bare_posixct:    return "VCTRS_TYPE2_S3_unspecified_bare_posixct";
  case VCTRS_TYPE2_S3_unspecified_bare_posixlt:    return "VCTRS_TYPE2_S3_unspecified_bare_posixlt";
  case VCTRS_TYPE2_S3_unspecified_bare_tibble:     return "VCTRS_TYPE2_S3_unspecified_bare_tibble";
  case VCTRS_TYPE2_S3_unspecified_unknown:         return "VCTRS_TYPE2_S3_unspecified_unknown";

  case VCTRS_TYPE2_S3_logical_bare_factor:         return "VCTRS_TYPE2_S3_logical_bare_factor";
  case VCTRS_TYPE2_S3_logical_bare_ordered:        return "VCTRS_TYPE2_S3_logical_bare_ordered";
  case VCTRS_TYPE2_S3_logical_bare_date:           return "VCTRS_TYPE2_S3_logical_bare_date";
  case VCTRS_TYPE2_S3_logical_bare_posixct:        return "VCTRS_TYPE2_S3_logical_bare_posixct";
  case VCTRS_TYPE2_S3_logical_bare_posixlt:        return "VCTRS_TYPE2_S3_logical_bare_posixlt";
  case VCTRS_TYPE2_S3_logical_bare_tibble:         return "VCTRS_TYPE2_S3_logical_bare_tibble";
  case VCTRS_TYPE2_S3_logical_unknown:             return "VCTRS_TYPE2_S3_logical_unknown";

  case VCTRS_TYPE2_S3_integer_bare_factor:         return "VCTRS_TYPE2_S3_integer_bare_factor";
  case VCTRS_TYPE2_S3_integer_bare_ordered:        return "VCTRS_TYPE2_S3_integer_bare_ordered";
  case VCTRS_TYPE2_S3_integer_bare_date:           return "VCTRS_TYPE2_S3_integer_bare_date";
  case VCTRS_TYPE2_S3_integer_bare_posixct:        return "VCTRS_TYPE2_S3_integer_bare_posixct";
  case VCTRS_TYPE2_S3_integer_bare_posixlt:        return "VCTRS_TYPE2_S3_integer_bare_posixlt";
  case VCTRS_TYPE2_S3_integer_bare_tibble:         return "VCTRS_TYPE2_S3_integer_bare_tibble";
  case VCTRS_TYPE2_S3_integer_unknown:             return "VCTRS_TYPE2_S3_integer_unknown";

  case VCTRS_TYPE2_S3_double_bare_factor:          return "VCTRS_TYPE2_S3_double_bare_factor";
  case VCTRS_TYPE2_S3_double_bare_ordered:         return "VCTRS_TYPE2_S3_double_bare_ordered";
  case VCTRS_TYPE2_S3_double_bare_date:            return "VCTRS_TYPE2_S3_double_bare_date";
  case VCTRS_TYPE2_S3_double_bare_posixct:         return "VCTRS_TYPE2_S3_double_bare_posixct";
  case VCTRS_TYPE2_S3_double_bare_posixlt:         return "VCTRS_TYPE2_S3_double_bare_posixlt";
  case VCTRS_TYPE2_S3_double_bare_tibble:          return "VCTRS_TYPE2_S3_double_bare_tibble";
  case VCTRS_TYPE2_S3_double_unknown:              return "VCTRS_TYPE2_S3_double_unknown";

  case VCTRS_TYPE2_S3_complex_bare_factor:         return "VCTRS_TYPE2_S3_complex_bare_factor";
  case VCTRS_TYPE2_S3_complex_bare_ordered:        return "VCTRS_TYPE2_S3_complex_bare_ordered";
  case VCTRS_TYPE2_S3_complex_bare_date:           return "VCTRS_TYPE2_S3_complex_bare_date";
  case VCTRS_TYPE2_S3_complex_bare_posixct:        return "VCTRS_TYPE2_S3_complex_bare_posixct";
  case VCTRS_TYPE2_S3_complex_bare_posixlt:        return "VCTRS_TYPE2_S3_complex_bare_posixlt";
  case VCTRS_TYPE2_S3_complex_bare_tibble:         return "VCTRS_TYPE2_S3_complex_bare_tibble";
  case VCTRS_TYPE2_S3_complex_unknown:             return "VCTRS_TYPE2_S3_complex_unknown";

  case VCTRS_TYPE2_S3_character_bare_factor:       return "VCTRS_TYPE2_S3_character_bare_factor";
  case VCTRS_TYPE2_S3_character_bare_ordered:      return "VCTRS_TYPE2_S3_character_bare_ordered";
  case VCTRS_TYPE2_S3_character_bare_date:         return "VCTRS_TYPE2_S3_character_bare_date";
  case VCTRS_TYPE2_S3_character_bare_posixct:      return "VCTRS_TYPE2_S3_character_bare_posixct";
  case VCTRS_TYPE2_S3_character_bare_posixlt:      return "VCTRS_TYPE2_S3_character_bare_posixlt";
  case VCTRS_TYPE2_S3_character_bare_tibble:       return "VCTRS_TYPE2_S3_character_bare_tibble";
  case VCTRS_TYPE2_S3_character_unknown:           return "VCTRS_TYPE2_S3_character_unknown";

  case VCTRS_TYPE2_S3_raw_bare_factor:             return "VCTRS_TYPE2_S3_raw_bare_factor";
  case VCTRS_TYPE2_S3_raw_bare_ordered:            return "VCTRS_TYPE2_S3_raw_bare_ordered";
  case VCTRS_TYPE2_S3_raw_bare_date:               return "VCTRS_TYPE2_S3_raw_bare_date";
  case VCTRS_TYPE2_S3_raw_bare_posixct:            return "VCTRS_TYPE2_S3_raw_bare_posixct";
  case VCTRS_TYPE2_S3_raw_bare_posixlt:            return "VCTRS_TYPE2_S3_raw_bare_posixlt";
  case VCTRS_TYPE2_S3_raw_bare_tibble:             return "VCTRS_TYPE2_S3_raw_bare_tibble";
  case VCTRS_TYPE2_S3_raw_unknown:                 return "VCTRS_TYPE2_S3_raw_unknown";

  case VCTRS_TYPE2_S3_list_bare_factor:            return "VCTRS_TYPE2_S3_list_bare_factor";
  case VCTRS_TYPE2_S3_list_bare_ordered:           return "VCTRS_TYPE2_S3_list_bare_ordered";
  case VCTRS_TYPE2_S3_list_bare_date:              return "VCTRS_TYPE2_S3_list_bare_date";
  case VCTRS_TYPE2_S3_list_bare_posixct:           return "VCTRS_TYPE2_S3_list_bare_posixct";
  case VCTRS_TYPE2_S3_list_bare_posixlt:           return "VCTRS_TYPE2_S3_list_bare_posixlt";
  case VCTRS_TYPE2_S3_list_bare_tibble:            return "VCTRS_TYPE2_S3_list_bare_tibble";
  case VCTRS_TYPE2_S3_list_unknown:                return "VCTRS_TYPE2_S3_list_unknown";

  case VCTRS_TYPE2_S3_dataframe_bare_factor:       return "VCTRS_TYPE2_S3_dataframe_bare_factor";
  case VCTRS_TYPE2_S3_dataframe_bare_ordered:      return "VCTRS_TYPE2_S3_dataframe_bare_ordered";
  case VCTRS_TYPE2_S3_dataframe_bare_date:         return "VCTRS_TYPE2_S3_dataframe_bare_date";
  case VCTRS_TYPE2_S3_dataframe_bare_posixct:      return "VCTRS_TYPE2_S3_dataframe_bare_posixct";
  case VCTRS_TYPE2_S3_dataframe_bare_posixlt:      return "VCTRS_TYPE2_S3_dataframe_bare_posixlt";
  case VCTRS_TYPE2_S3_dataframe_bare_tibble:       return "VCTRS_TYPE2_S3_dataframe_bare_tibble";
  case VCTRS_TYPE2_S3_dataframe_unknown:           return "VCTRS_TYPE2_S3_dataframe_unknown";

  case VCTRS_TYPE2_S3_scalar_bare_factor:          return "VCTRS_TYPE2_S3_scalar_bare_factor";
  case VCTRS_TYPE2_S3_scalar_bare_ordered:         return "VCTRS_TYPE2_S3_scalar_bare_ordered";
  case VCTRS_TYPE2_S3_scalar_bare_date:            return "VCTRS_TYPE2_S3_scalar_bare_date";
  case VCTRS_TYPE2_S3_scalar_bare_posixct:         return "VCTRS_TYPE2_S3_scalar_bare_posixct";
  case VCTRS_TYPE2_S3_scalar_bare_posixlt:         return "VCTRS_TYPE2_S3_scalar_bare_posixlt";
  case VCTRS_TYPE2_S3_scalar_bare_tibble:          return "VCTRS_TYPE2_S3_scalar_bare_tibble";
  case VCTRS_TYPE2_S3_scalar_unknown:              return "VCTRS_TYPE2_S3_scalar_unknown";

  case VCTRS_TYPE2_S3_bare_factor_bare_factor:     return "VCTRS_TYPE2_S3_bare_factor_bare_factor";
  case VCTRS_TYPE2_S3_bare_factor_bare_ordered:    return "VCTRS_TYPE2_S3_bare_factor_bare_ordered";
  case VCTRS_TYPE2_S3_bare_factor_bare_date:       return "VCTRS_TYPE2_S3_bare_factor_bare_date";
  case VCTRS_TYPE2_S3_bare_factor_bare_posixct:    return "VCTRS_TYPE2_S3_bare_factor_bare_posixct";
  case VCTRS_TYPE2_S3_bare_factor_bare_posixlt:    return "VCTRS_TYPE2_S3_bare_factor_bare_posixlt";
  case VCTRS_TYPE2_S3_bare_factor_bare_tibble:     return "VCTRS_TYPE2_S3_bare_factor_bare_tibble";
  case VCTRS_TYPE2_S3_bare_factor_unknown:         return "VCTRS_TYPE2_S3_bare_factor_unknown";

  case VCTRS_TYPE2_S3_bare_ordered_bare_ordered:   return "VCTRS_TYPE2_S3_bare_ordered_bare_ordered";
  case VCTRS_TYPE2_S3_bare_ordered_bare_date:      return "VCTRS_TYPE2_S3_bare_ordered_bare_date";
  case VCTRS_TYPE2_S3_bare_ordered_bare_posixct:   return "VCTRS_TYPE2_S3_bare_ordered_bare_posixct";
  case VCTRS_TYPE2_S3_bare_ordered_bare_posixlt:   return "VCTRS_TYPE2_S3_bare_ordered_bare_posixlt";
  case VCTRS_TYPE2_S3_bare_ordered_bare_tibble:    return "VCTRS_TYPE2_S3_bare_ordered_bare_tibble";
  case VCTRS_TYPE2_S3_bare_ordered_unknown:        return "VCTRS_TYPE2_S3_bare_ordered_unknown";

  case VCTRS_TYPE2_S3_bare_date_bare_date:         return "VCTRS_TYPE2_S3_bare_date_bare_date";
  case VCTRS_TYPE2_S3_bare_date_bare_posixct:      return "VCTRS_TYPE2_S3_bare_date_bare_posixct";
  case VCTRS_TYPE2_S3_bare_date_bare_posixlt:      return "VCTRS_TYPE2_S3_bare_date_bare_posixlt";
  case VCTRS_TYPE2_S3_bare_date_bare_tibble:       return "VCTRS_TYPE2_S3_bare_date_bare_tibble";
  case VCTRS_TYPE2_S3_bare_date_unknown:           return "VCTRS_TYPE2_S3_bare_date_unknown";

  case VCTRS_TYPE2_S3_bare_posixct_bare_posixct:   return "VCTRS_TYPE2_S3_bare_posixct_bare_posixct";
  case VCTRS_TYPE2_S3_bare_posixct_bare_posixlt:   return "VCTRS_TYPE2_S3_bare_posixct_bare_posixlt";
  case VCTRS_TYPE2_S3_bare_posixct_bare_tibble:    return "VCTRS_TYPE2_S3_bare_posixct_bare_tibble";
  case VCTRS_TYPE2_S3_bare_posixct_unknown:        return "VCTRS_TYPE2_S3_bare_posixct_unknown";

  case VCTRS_TYPE2_S3_bare_posixlt_bare_posixlt:   return "VCTRS_TYPE2_S3_bare_posixlt_bare_posixlt";
  case VCTRS_TYPE2_S3_bare_posixlt_bare_tibble:    return "VCTRS_TYPE2_S3_bare_posixlt_bare_tibble";
  case VCTRS_TYPE2_S3_bare_posixlt_unknown:        return "VCTRS_TYPE2_S3_bare_posixlt_unknown";

  case VCTRS_TYPE2_S3_bare_tibble_bare_tibble:     return "VCTRS_TYPE2_S3_bare_tibble_bare_tibble";
  case VCTRS_TYPE2_S3_bare_tibble_unknown:         return "VCTRS_TYPE2_S3_bare_tibble_unknown";

  case VCTRS_TYPE2_S3_unknown_unknown:             return "VCTRS_TYPE2_S3_unknown_unknown";
  }

  r_stop_unreachable();
}

r_obj* ffi_typeof2_s3(r_obj* x, r_obj* y) {
  enum vctrs_type2_s3 type = vec_typeof2_s3(x, y);
  return r_chr(vctrs_type2_s3_as_str(type));
}
