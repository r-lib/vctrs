#include <rlang.h>
#include "vctrs.h"
#include "utils.h"

static enum vctrs_type2_s3 vec_typeof2_s3_impl2(SEXP x,
                                                SEXP y,
                                                enum vctrs_type type_y,
                                                int* left);

// [[ include("ptype2.h") ]]
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
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_null_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_null_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_null_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_null_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_null_unknown;
    }
  }
  case vctrs_type_unspecified: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_unspecified_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_unspecified_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_unspecified_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_unspecified_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_unspecified_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_unspecified_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_unspecified_unknown;
    }
  }
  case vctrs_type_logical: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_logical_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_logical_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_logical_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_logical_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_logical_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_logical_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_logical_unknown;
    }
  }
  case vctrs_type_integer: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_integer_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_integer_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_integer_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_integer_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_integer_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_integer_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_integer_unknown;
    }
  }
  case vctrs_type_double: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_double_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_double_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_double_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_double_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_double_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_double_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_double_unknown;
    }
  }
  case vctrs_type_complex: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_complex_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_complex_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_complex_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_complex_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_complex_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_complex_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_complex_unknown;
    }
  }
  case vctrs_type_character: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_character_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_character_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_character_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_character_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_character_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_character_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_character_unknown;
    }
  }
  case vctrs_type_raw: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_raw_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_raw_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_raw_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_raw_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_raw_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_raw_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_raw_unknown;
    }
  }
  case vctrs_type_list: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_list_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_list_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_list_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_list_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_list_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_list_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_list_unknown;
    }
  }
  case vctrs_type_dataframe: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_dataframe_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_dataframe_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_dataframe_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_dataframe_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_dataframe_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_dataframe_bare_tibble;
    default:                       *left = 0; return vctrs_type2_s3_dataframe_unknown;
    }
  }
  case vctrs_type_scalar: {
    switch (class_type(y)) {
    case vctrs_class_bare_factor:  *left = 0; return vctrs_type2_s3_scalar_bare_factor;
    case vctrs_class_bare_ordered: *left = 0; return vctrs_type2_s3_scalar_bare_ordered;
    case vctrs_class_bare_date:    *left = 0; return vctrs_type2_s3_scalar_bare_date;
    case vctrs_class_bare_posixct: *left = 0; return vctrs_type2_s3_scalar_bare_posixct;
    case vctrs_class_bare_posixlt: *left = 0; return vctrs_type2_s3_scalar_bare_posixlt;
    case vctrs_class_bare_tibble:  *left = 0; return vctrs_type2_s3_scalar_bare_tibble;
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
    case vctrs_type_unspecified:     *left =  1; return vctrs_type2_s3_unspecified_bare_factor;
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
      case vctrs_class_bare_date:    *left =  0; return vctrs_type2_s3_bare_factor_bare_date;
      case vctrs_class_bare_posixct: *left =  0; return vctrs_type2_s3_bare_factor_bare_posixct;
      case vctrs_class_bare_posixlt: *left =  0; return vctrs_type2_s3_bare_factor_bare_posixlt;
      case vctrs_class_bare_tibble:  *left =  0; return vctrs_type2_s3_bare_factor_bare_tibble;
      default:                       *left =  0; return vctrs_type2_s3_bare_factor_unknown;
      }
    }}
  }
  case vctrs_class_bare_ordered: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_bare_ordered;
    case vctrs_type_unspecified:     *left =  1; return vctrs_type2_s3_unspecified_bare_ordered;
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
      case vctrs_class_bare_date:    *left =  0; return vctrs_type2_s3_bare_ordered_bare_date;
      case vctrs_class_bare_posixct: *left =  0; return vctrs_type2_s3_bare_ordered_bare_posixct;
      case vctrs_class_bare_posixlt: *left =  0; return vctrs_type2_s3_bare_ordered_bare_posixlt;
      case vctrs_class_bare_tibble:  *left =  0; return vctrs_type2_s3_bare_ordered_bare_tibble;
      default:                       *left =  0; return vctrs_type2_s3_bare_ordered_unknown;
      }
    }}
  }
  case vctrs_class_bare_date: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_bare_date;
    case vctrs_type_unspecified:     *left =  1; return vctrs_type2_s3_unspecified_bare_date;
    case vctrs_type_logical:         *left =  1; return vctrs_type2_s3_logical_bare_date;
    case vctrs_type_integer:         *left =  1; return vctrs_type2_s3_integer_bare_date;
    case vctrs_type_double:          *left =  1; return vctrs_type2_s3_double_bare_date;
    case vctrs_type_complex:         *left =  1; return vctrs_type2_s3_complex_bare_date;
    case vctrs_type_character:       *left =  1; return vctrs_type2_s3_character_bare_date;
    case vctrs_type_raw:             *left =  1; return vctrs_type2_s3_raw_bare_date;
    case vctrs_type_list:            *left =  1; return vctrs_type2_s3_list_bare_date;
    case vctrs_type_dataframe:       *left =  1; return vctrs_type2_s3_dataframe_bare_date;
    case vctrs_type_scalar:          *left =  1; return vctrs_type2_s3_scalar_bare_date;
    case vctrs_type_s3: {
      switch (class_type(y)) {
      case vctrs_class_bare_factor:  *left =  1; return vctrs_type2_s3_bare_factor_bare_date;
      case vctrs_class_bare_ordered: *left =  1; return vctrs_type2_s3_bare_ordered_bare_date;
      case vctrs_class_bare_date:    *left = -1; return vctrs_type2_s3_bare_date_bare_date;
      case vctrs_class_bare_posixct: *left =  0; return vctrs_type2_s3_bare_date_bare_posixct;
      case vctrs_class_bare_posixlt: *left =  0; return vctrs_type2_s3_bare_date_bare_posixlt;
      case vctrs_class_bare_tibble:  *left =  0; return vctrs_type2_s3_bare_date_bare_tibble;
      default:                       *left =  0; return vctrs_type2_s3_bare_date_unknown;
      }
    }}
  }
  case vctrs_class_bare_posixct: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_bare_posixct;
    case vctrs_type_unspecified:     *left =  1; return vctrs_type2_s3_unspecified_bare_posixct;
    case vctrs_type_logical:         *left =  1; return vctrs_type2_s3_logical_bare_posixct;
    case vctrs_type_integer:         *left =  1; return vctrs_type2_s3_integer_bare_posixct;
    case vctrs_type_double:          *left =  1; return vctrs_type2_s3_double_bare_posixct;
    case vctrs_type_complex:         *left =  1; return vctrs_type2_s3_complex_bare_posixct;
    case vctrs_type_character:       *left =  1; return vctrs_type2_s3_character_bare_posixct;
    case vctrs_type_raw:             *left =  1; return vctrs_type2_s3_raw_bare_posixct;
    case vctrs_type_list:            *left =  1; return vctrs_type2_s3_list_bare_posixct;
    case vctrs_type_dataframe:       *left =  1; return vctrs_type2_s3_dataframe_bare_posixct;
    case vctrs_type_scalar:          *left =  1; return vctrs_type2_s3_scalar_bare_posixct;
    case vctrs_type_s3: {
      switch (class_type(y)) {
      case vctrs_class_bare_factor:  *left =  1; return vctrs_type2_s3_bare_factor_bare_posixct;
      case vctrs_class_bare_ordered: *left =  1; return vctrs_type2_s3_bare_ordered_bare_posixct;
      case vctrs_class_bare_date:    *left =  1; return vctrs_type2_s3_bare_date_bare_posixct;
      case vctrs_class_bare_posixct: *left = -1; return vctrs_type2_s3_bare_posixct_bare_posixct;
      case vctrs_class_bare_posixlt: *left =  0; return vctrs_type2_s3_bare_posixct_bare_posixlt;
      case vctrs_class_bare_tibble:  *left =  0; return vctrs_type2_s3_bare_posixct_bare_tibble;
      default:                       *left =  0; return vctrs_type2_s3_bare_posixct_unknown;
      }
    }}
  }
  case vctrs_class_bare_posixlt: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_bare_posixlt;
    case vctrs_type_unspecified:     *left =  1; return vctrs_type2_s3_unspecified_bare_posixlt;
    case vctrs_type_logical:         *left =  1; return vctrs_type2_s3_logical_bare_posixlt;
    case vctrs_type_integer:         *left =  1; return vctrs_type2_s3_integer_bare_posixlt;
    case vctrs_type_double:          *left =  1; return vctrs_type2_s3_double_bare_posixlt;
    case vctrs_type_complex:         *left =  1; return vctrs_type2_s3_complex_bare_posixlt;
    case vctrs_type_character:       *left =  1; return vctrs_type2_s3_character_bare_posixlt;
    case vctrs_type_raw:             *left =  1; return vctrs_type2_s3_raw_bare_posixlt;
    case vctrs_type_list:            *left =  1; return vctrs_type2_s3_list_bare_posixlt;
    case vctrs_type_dataframe:       *left =  1; return vctrs_type2_s3_dataframe_bare_posixlt;
    case vctrs_type_scalar:          *left =  1; return vctrs_type2_s3_scalar_bare_posixlt;
    case vctrs_type_s3: {
      switch (class_type(y)) {
      case vctrs_class_bare_factor:  *left =  1; return vctrs_type2_s3_bare_factor_bare_posixlt;
      case vctrs_class_bare_ordered: *left =  1; return vctrs_type2_s3_bare_ordered_bare_posixlt;
      case vctrs_class_bare_date:    *left =  1; return vctrs_type2_s3_bare_date_bare_posixlt;
      case vctrs_class_bare_posixct: *left =  1; return vctrs_type2_s3_bare_posixct_bare_posixlt;
      case vctrs_class_bare_posixlt: *left = -1; return vctrs_type2_s3_bare_posixlt_bare_posixlt;
      case vctrs_class_bare_tibble:  *left =  0; return vctrs_type2_s3_bare_posixlt_bare_tibble;
      default:                       *left =  0; return vctrs_type2_s3_bare_posixlt_unknown;
      }
    }}
  }
  case vctrs_class_bare_tibble: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_bare_tibble;
    case vctrs_type_unspecified:     *left =  1; return vctrs_type2_s3_unspecified_bare_tibble;
    case vctrs_type_logical:         *left =  1; return vctrs_type2_s3_logical_bare_tibble;
    case vctrs_type_integer:         *left =  1; return vctrs_type2_s3_integer_bare_tibble;
    case vctrs_type_double:          *left =  1; return vctrs_type2_s3_double_bare_tibble;
    case vctrs_type_complex:         *left =  1; return vctrs_type2_s3_complex_bare_tibble;
    case vctrs_type_character:       *left =  1; return vctrs_type2_s3_character_bare_tibble;
    case vctrs_type_raw:             *left =  1; return vctrs_type2_s3_raw_bare_tibble;
    case vctrs_type_list:            *left =  1; return vctrs_type2_s3_list_bare_tibble;
    case vctrs_type_dataframe:       *left =  1; return vctrs_type2_s3_dataframe_bare_tibble;
    case vctrs_type_scalar:          *left =  1; return vctrs_type2_s3_scalar_bare_tibble;
    case vctrs_type_s3: {
      switch (class_type(y)) {
      case vctrs_class_bare_factor:  *left =  1; return vctrs_type2_s3_bare_factor_bare_tibble;
      case vctrs_class_bare_ordered: *left =  1; return vctrs_type2_s3_bare_ordered_bare_tibble;
      case vctrs_class_bare_date:    *left =  1; return vctrs_type2_s3_bare_date_bare_tibble;
      case vctrs_class_bare_posixct: *left =  1; return vctrs_type2_s3_bare_posixct_bare_tibble;
      case vctrs_class_bare_posixlt: *left =  1; return vctrs_type2_s3_bare_posixlt_bare_tibble;
      case vctrs_class_bare_tibble:  *left = -1; return vctrs_type2_s3_bare_tibble_bare_tibble;
      default:                       *left =  0; return vctrs_type2_s3_bare_tibble_unknown;
      }
    }}
  }
  default: {
    switch (type_y) {
    case vctrs_type_null:            *left =  1; return vctrs_type2_s3_null_unknown;
    case vctrs_type_unspecified:     *left =  1; return vctrs_type2_s3_unspecified_unknown;
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
      case vctrs_class_bare_date:    *left =  1; return vctrs_type2_s3_bare_date_unknown;
      case vctrs_class_bare_posixct: *left =  1; return vctrs_type2_s3_bare_posixct_unknown;
      case vctrs_class_bare_posixlt: *left =  1; return vctrs_type2_s3_bare_posixlt_unknown;
      case vctrs_class_bare_tibble:  *left =  1; return vctrs_type2_s3_bare_tibble_unknown;
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
  case vctrs_type2_s3_null_bare_date:              return "vctrs_type2_s3_null_bare_date";
  case vctrs_type2_s3_null_bare_posixct:           return "vctrs_type2_s3_null_bare_posixct";
  case vctrs_type2_s3_null_bare_posixlt:           return "vctrs_type2_s3_null_bare_posixlt";
  case vctrs_type2_s3_null_bare_tibble:            return "vctrs_type2_s3_null_bare_tibble";
  case vctrs_type2_s3_null_unknown:                return "vctrs_type2_s3_null_unknown";

  case vctrs_type2_s3_unspecified_bare_factor:     return "vctrs_type2_s3_unspecified_bare_factor";
  case vctrs_type2_s3_unspecified_bare_ordered:    return "vctrs_type2_s3_unspecified_bare_ordered";
  case vctrs_type2_s3_unspecified_bare_date:       return "vctrs_type2_s3_unspecified_bare_date";
  case vctrs_type2_s3_unspecified_bare_posixct:    return "vctrs_type2_s3_unspecified_bare_posixct";
  case vctrs_type2_s3_unspecified_bare_posixlt:    return "vctrs_type2_s3_unspecified_bare_posixlt";
  case vctrs_type2_s3_unspecified_bare_tibble:     return "vctrs_type2_s3_unspecified_bare_tibble";
  case vctrs_type2_s3_unspecified_unknown:         return "vctrs_type2_s3_unspecified_unknown";

  case vctrs_type2_s3_logical_bare_factor:         return "vctrs_type2_s3_logical_bare_factor";
  case vctrs_type2_s3_logical_bare_ordered:        return "vctrs_type2_s3_logical_bare_ordered";
  case vctrs_type2_s3_logical_bare_date:           return "vctrs_type2_s3_logical_bare_date";
  case vctrs_type2_s3_logical_bare_posixct:        return "vctrs_type2_s3_logical_bare_posixct";
  case vctrs_type2_s3_logical_bare_posixlt:        return "vctrs_type2_s3_logical_bare_posixlt";
  case vctrs_type2_s3_logical_bare_tibble:         return "vctrs_type2_s3_logical_bare_tibble";
  case vctrs_type2_s3_logical_unknown:             return "vctrs_type2_s3_logical_unknown";

  case vctrs_type2_s3_integer_bare_factor:         return "vctrs_type2_s3_integer_bare_factor";
  case vctrs_type2_s3_integer_bare_ordered:        return "vctrs_type2_s3_integer_bare_ordered";
  case vctrs_type2_s3_integer_bare_date:           return "vctrs_type2_s3_integer_bare_date";
  case vctrs_type2_s3_integer_bare_posixct:        return "vctrs_type2_s3_integer_bare_posixct";
  case vctrs_type2_s3_integer_bare_posixlt:        return "vctrs_type2_s3_integer_bare_posixlt";
  case vctrs_type2_s3_integer_bare_tibble:         return "vctrs_type2_s3_integer_bare_tibble";
  case vctrs_type2_s3_integer_unknown:             return "vctrs_type2_s3_integer_unknown";

  case vctrs_type2_s3_double_bare_factor:          return "vctrs_type2_s3_double_bare_factor";
  case vctrs_type2_s3_double_bare_ordered:         return "vctrs_type2_s3_double_bare_ordered";
  case vctrs_type2_s3_double_bare_date:            return "vctrs_type2_s3_double_bare_date";
  case vctrs_type2_s3_double_bare_posixct:         return "vctrs_type2_s3_double_bare_posixct";
  case vctrs_type2_s3_double_bare_posixlt:         return "vctrs_type2_s3_double_bare_posixlt";
  case vctrs_type2_s3_double_bare_tibble:          return "vctrs_type2_s3_double_bare_tibble";
  case vctrs_type2_s3_double_unknown:              return "vctrs_type2_s3_double_unknown";

  case vctrs_type2_s3_complex_bare_factor:         return "vctrs_type2_s3_complex_bare_factor";
  case vctrs_type2_s3_complex_bare_ordered:        return "vctrs_type2_s3_complex_bare_ordered";
  case vctrs_type2_s3_complex_bare_date:           return "vctrs_type2_s3_complex_bare_date";
  case vctrs_type2_s3_complex_bare_posixct:        return "vctrs_type2_s3_complex_bare_posixct";
  case vctrs_type2_s3_complex_bare_posixlt:        return "vctrs_type2_s3_complex_bare_posixlt";
  case vctrs_type2_s3_complex_bare_tibble:         return "vctrs_type2_s3_complex_bare_tibble";
  case vctrs_type2_s3_complex_unknown:             return "vctrs_type2_s3_complex_unknown";

  case vctrs_type2_s3_character_bare_factor:       return "vctrs_type2_s3_character_bare_factor";
  case vctrs_type2_s3_character_bare_ordered:      return "vctrs_type2_s3_character_bare_ordered";
  case vctrs_type2_s3_character_bare_date:         return "vctrs_type2_s3_character_bare_date";
  case vctrs_type2_s3_character_bare_posixct:      return "vctrs_type2_s3_character_bare_posixct";
  case vctrs_type2_s3_character_bare_posixlt:      return "vctrs_type2_s3_character_bare_posixlt";
  case vctrs_type2_s3_character_bare_tibble:       return "vctrs_type2_s3_character_bare_tibble";
  case vctrs_type2_s3_character_unknown:           return "vctrs_type2_s3_character_unknown";

  case vctrs_type2_s3_raw_bare_factor:             return "vctrs_type2_s3_raw_bare_factor";
  case vctrs_type2_s3_raw_bare_ordered:            return "vctrs_type2_s3_raw_bare_ordered";
  case vctrs_type2_s3_raw_bare_date:               return "vctrs_type2_s3_raw_bare_date";
  case vctrs_type2_s3_raw_bare_posixct:            return "vctrs_type2_s3_raw_bare_posixct";
  case vctrs_type2_s3_raw_bare_posixlt:            return "vctrs_type2_s3_raw_bare_posixlt";
  case vctrs_type2_s3_raw_bare_tibble:             return "vctrs_type2_s3_raw_bare_tibble";
  case vctrs_type2_s3_raw_unknown:                 return "vctrs_type2_s3_raw_unknown";

  case vctrs_type2_s3_list_bare_factor:            return "vctrs_type2_s3_list_bare_factor";
  case vctrs_type2_s3_list_bare_ordered:           return "vctrs_type2_s3_list_bare_ordered";
  case vctrs_type2_s3_list_bare_date:              return "vctrs_type2_s3_list_bare_date";
  case vctrs_type2_s3_list_bare_posixct:           return "vctrs_type2_s3_list_bare_posixct";
  case vctrs_type2_s3_list_bare_posixlt:           return "vctrs_type2_s3_list_bare_posixlt";
  case vctrs_type2_s3_list_bare_tibble:            return "vctrs_type2_s3_list_bare_tibble";
  case vctrs_type2_s3_list_unknown:                return "vctrs_type2_s3_list_unknown";

  case vctrs_type2_s3_dataframe_bare_factor:       return "vctrs_type2_s3_dataframe_bare_factor";
  case vctrs_type2_s3_dataframe_bare_ordered:      return "vctrs_type2_s3_dataframe_bare_ordered";
  case vctrs_type2_s3_dataframe_bare_date:         return "vctrs_type2_s3_dataframe_bare_date";
  case vctrs_type2_s3_dataframe_bare_posixct:      return "vctrs_type2_s3_dataframe_bare_posixct";
  case vctrs_type2_s3_dataframe_bare_posixlt:      return "vctrs_type2_s3_dataframe_bare_posixlt";
  case vctrs_type2_s3_dataframe_bare_tibble:       return "vctrs_type2_s3_dataframe_bare_tibble";
  case vctrs_type2_s3_dataframe_unknown:           return "vctrs_type2_s3_dataframe_unknown";

  case vctrs_type2_s3_scalar_bare_factor:          return "vctrs_type2_s3_scalar_bare_factor";
  case vctrs_type2_s3_scalar_bare_ordered:         return "vctrs_type2_s3_scalar_bare_ordered";
  case vctrs_type2_s3_scalar_bare_date:            return "vctrs_type2_s3_scalar_bare_date";
  case vctrs_type2_s3_scalar_bare_posixct:         return "vctrs_type2_s3_scalar_bare_posixct";
  case vctrs_type2_s3_scalar_bare_posixlt:         return "vctrs_type2_s3_scalar_bare_posixlt";
  case vctrs_type2_s3_scalar_bare_tibble:          return "vctrs_type2_s3_scalar_bare_tibble";
  case vctrs_type2_s3_scalar_unknown:              return "vctrs_type2_s3_scalar_unknown";

  case vctrs_type2_s3_bare_factor_bare_factor:     return "vctrs_type2_s3_bare_factor_bare_factor";
  case vctrs_type2_s3_bare_factor_bare_ordered:    return "vctrs_type2_s3_bare_factor_bare_ordered";
  case vctrs_type2_s3_bare_factor_bare_date:       return "vctrs_type2_s3_bare_factor_bare_date";
  case vctrs_type2_s3_bare_factor_bare_posixct:    return "vctrs_type2_s3_bare_factor_bare_posixct";
  case vctrs_type2_s3_bare_factor_bare_posixlt:    return "vctrs_type2_s3_bare_factor_bare_posixlt";
  case vctrs_type2_s3_bare_factor_bare_tibble:     return "vctrs_type2_s3_bare_factor_bare_tibble";
  case vctrs_type2_s3_bare_factor_unknown:         return "vctrs_type2_s3_bare_factor_unknown";

  case vctrs_type2_s3_bare_ordered_bare_ordered:   return "vctrs_type2_s3_bare_ordered_bare_ordered";
  case vctrs_type2_s3_bare_ordered_bare_date:      return "vctrs_type2_s3_bare_ordered_bare_date";
  case vctrs_type2_s3_bare_ordered_bare_posixct:   return "vctrs_type2_s3_bare_ordered_bare_posixct";
  case vctrs_type2_s3_bare_ordered_bare_posixlt:   return "vctrs_type2_s3_bare_ordered_bare_posixlt";
  case vctrs_type2_s3_bare_ordered_bare_tibble:    return "vctrs_type2_s3_bare_ordered_bare_tibble";
  case vctrs_type2_s3_bare_ordered_unknown:        return "vctrs_type2_s3_bare_ordered_unknown";

  case vctrs_type2_s3_bare_date_bare_date:         return "vctrs_type2_s3_bare_date_bare_date";
  case vctrs_type2_s3_bare_date_bare_posixct:      return "vctrs_type2_s3_bare_date_bare_posixct";
  case vctrs_type2_s3_bare_date_bare_posixlt:      return "vctrs_type2_s3_bare_date_bare_posixlt";
  case vctrs_type2_s3_bare_date_bare_tibble:       return "vctrs_type2_s3_bare_date_bare_tibble";
  case vctrs_type2_s3_bare_date_unknown:           return "vctrs_type2_s3_bare_date_unknown";

  case vctrs_type2_s3_bare_posixct_bare_posixct:   return "vctrs_type2_s3_bare_posixct_bare_posixct";
  case vctrs_type2_s3_bare_posixct_bare_posixlt:   return "vctrs_type2_s3_bare_posixct_bare_posixlt";
  case vctrs_type2_s3_bare_posixct_bare_tibble:    return "vctrs_type2_s3_bare_posixct_bare_tibble";
  case vctrs_type2_s3_bare_posixct_unknown:        return "vctrs_type2_s3_bare_posixct_unknown";

  case vctrs_type2_s3_bare_posixlt_bare_posixlt:   return "vctrs_type2_s3_bare_posixlt_bare_posixlt";
  case vctrs_type2_s3_bare_posixlt_bare_tibble:    return "vctrs_type2_s3_bare_posixlt_bare_tibble";
  case vctrs_type2_s3_bare_posixlt_unknown:        return "vctrs_type2_s3_bare_posixlt_unknown";

  case vctrs_type2_s3_bare_tibble_bare_tibble:     return "vctrs_type2_s3_bare_tibble_bare_tibble";
  case vctrs_type2_s3_bare_tibble_unknown:         return "vctrs_type2_s3_bare_tibble_unknown";

  case vctrs_type2_s3_unknown_unknown:             return "vctrs_type2_s3_unknown_unknown";
  }

  never_reached("vctrs_type2_s3_as_str");
}

// [[ register() ]]
SEXP vctrs_typeof2_s3(SEXP x, SEXP y) {
  enum vctrs_type2_s3 type = vec_typeof2_s3(x, y);
  return Rf_mkString(vctrs_type2_s3_as_str(type));
}
