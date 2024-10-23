#ifndef VCTRS_TYPEOF2_H
#define VCTRS_TYPEOF2_H

#include "vctrs-core.h"


enum vctrs_type2 {
  VCTRS_TYPE2_null_null,
  VCTRS_TYPE2_null_unspecified,
  VCTRS_TYPE2_null_logical,
  VCTRS_TYPE2_null_integer,
  VCTRS_TYPE2_null_double,
  VCTRS_TYPE2_null_complex,
  VCTRS_TYPE2_null_character,
  VCTRS_TYPE2_null_raw,
  VCTRS_TYPE2_null_list,
  VCTRS_TYPE2_null_dataframe,
  VCTRS_TYPE2_null_s3,
  VCTRS_TYPE2_null_scalar,

  VCTRS_TYPE2_unspecified_unspecified,
  VCTRS_TYPE2_unspecified_logical,
  VCTRS_TYPE2_unspecified_integer,
  VCTRS_TYPE2_unspecified_double,
  VCTRS_TYPE2_unspecified_complex,
  VCTRS_TYPE2_unspecified_character,
  VCTRS_TYPE2_unspecified_raw,
  VCTRS_TYPE2_unspecified_list,
  VCTRS_TYPE2_unspecified_dataframe,
  VCTRS_TYPE2_unspecified_s3,
  VCTRS_TYPE2_unspecified_scalar,

  VCTRS_TYPE2_logical_logical,
  VCTRS_TYPE2_logical_integer,
  VCTRS_TYPE2_logical_double,
  VCTRS_TYPE2_logical_complex,
  VCTRS_TYPE2_logical_character,
  VCTRS_TYPE2_logical_raw,
  VCTRS_TYPE2_logical_list,
  VCTRS_TYPE2_logical_dataframe,
  VCTRS_TYPE2_logical_s3,
  VCTRS_TYPE2_logical_scalar,

  VCTRS_TYPE2_integer_integer,
  VCTRS_TYPE2_integer_double,
  VCTRS_TYPE2_integer_complex,
  VCTRS_TYPE2_integer_character,
  VCTRS_TYPE2_integer_raw,
  VCTRS_TYPE2_integer_list,
  VCTRS_TYPE2_integer_dataframe,
  VCTRS_TYPE2_integer_s3,
  VCTRS_TYPE2_integer_scalar,

  VCTRS_TYPE2_double_double,
  VCTRS_TYPE2_double_complex,
  VCTRS_TYPE2_double_character,
  VCTRS_TYPE2_double_raw,
  VCTRS_TYPE2_double_list,
  VCTRS_TYPE2_double_dataframe,
  VCTRS_TYPE2_double_s3,
  VCTRS_TYPE2_double_scalar,

  VCTRS_TYPE2_complex_complex,
  VCTRS_TYPE2_complex_character,
  VCTRS_TYPE2_complex_raw,
  VCTRS_TYPE2_complex_list,
  VCTRS_TYPE2_complex_dataframe,
  VCTRS_TYPE2_complex_s3,
  VCTRS_TYPE2_complex_scalar,

  VCTRS_TYPE2_character_character,
  VCTRS_TYPE2_character_raw,
  VCTRS_TYPE2_character_list,
  VCTRS_TYPE2_character_dataframe,
  VCTRS_TYPE2_character_s3,
  VCTRS_TYPE2_character_scalar,

  VCTRS_TYPE2_raw_raw,
  VCTRS_TYPE2_raw_list,
  VCTRS_TYPE2_raw_dataframe,
  VCTRS_TYPE2_raw_s3,
  VCTRS_TYPE2_raw_scalar,

  VCTRS_TYPE2_list_list,
  VCTRS_TYPE2_list_dataframe,
  VCTRS_TYPE2_list_s3,
  VCTRS_TYPE2_list_scalar,

  VCTRS_TYPE2_dataframe_dataframe,
  VCTRS_TYPE2_dataframe_s3,
  VCTRS_TYPE2_dataframe_scalar,

  VCTRS_TYPE2_S3_s3,
  VCTRS_TYPE2_S3_scalar,

  VCTRS_TYPE2_scalar_scalar
};


enum vctrs_type2 vec_typeof2_impl(enum vctrs_type type_x,
                                  enum vctrs_type type_y,
                                  int* left);

enum vctrs_type2 vec_typeof2(r_obj* x, r_obj* y);
const char* vctrs_type2_as_str(enum vctrs_type2 type);


#endif
