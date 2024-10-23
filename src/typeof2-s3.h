#ifndef VCTRS_TYPEOF2_S3_H
#define VCTRS_TYPEOF2_S3_H

#include "vctrs-core.h"


enum vctrs_type2_s3 {
  VCTRS_TYPE2_S3_null_bare_factor,
  VCTRS_TYPE2_S3_null_bare_ordered,
  VCTRS_TYPE2_S3_null_bare_date,
  VCTRS_TYPE2_S3_null_bare_posixct,
  VCTRS_TYPE2_S3_null_bare_posixlt,
  VCTRS_TYPE2_S3_null_bare_tibble,
  VCTRS_TYPE2_S3_null_unknown,

  VCTRS_TYPE2_S3_unspecified_bare_factor,
  VCTRS_TYPE2_S3_unspecified_bare_ordered,
  VCTRS_TYPE2_S3_unspecified_bare_date,
  VCTRS_TYPE2_S3_unspecified_bare_posixct,
  VCTRS_TYPE2_S3_unspecified_bare_posixlt,
  VCTRS_TYPE2_S3_unspecified_bare_tibble,
  VCTRS_TYPE2_S3_unspecified_unknown,

  VCTRS_TYPE2_S3_logical_bare_factor,
  VCTRS_TYPE2_S3_logical_bare_ordered,
  VCTRS_TYPE2_S3_logical_bare_date,
  VCTRS_TYPE2_S3_logical_bare_posixct,
  VCTRS_TYPE2_S3_logical_bare_posixlt,
  VCTRS_TYPE2_S3_logical_bare_tibble,
  VCTRS_TYPE2_S3_logical_unknown,

  VCTRS_TYPE2_S3_integer_bare_factor,
  VCTRS_TYPE2_S3_integer_bare_ordered,
  VCTRS_TYPE2_S3_integer_bare_date,
  VCTRS_TYPE2_S3_integer_bare_posixct,
  VCTRS_TYPE2_S3_integer_bare_posixlt,
  VCTRS_TYPE2_S3_integer_bare_tibble,
  VCTRS_TYPE2_S3_integer_unknown,

  VCTRS_TYPE2_S3_double_bare_factor,
  VCTRS_TYPE2_S3_double_bare_ordered,
  VCTRS_TYPE2_S3_double_bare_date,
  VCTRS_TYPE2_S3_double_bare_posixct,
  VCTRS_TYPE2_S3_double_bare_posixlt,
  VCTRS_TYPE2_S3_double_bare_tibble,
  VCTRS_TYPE2_S3_double_unknown,

  VCTRS_TYPE2_S3_complex_bare_factor,
  VCTRS_TYPE2_S3_complex_bare_ordered,
  VCTRS_TYPE2_S3_complex_bare_date,
  VCTRS_TYPE2_S3_complex_bare_posixct,
  VCTRS_TYPE2_S3_complex_bare_posixlt,
  VCTRS_TYPE2_S3_complex_bare_tibble,
  VCTRS_TYPE2_S3_complex_unknown,

  VCTRS_TYPE2_S3_character_bare_factor,
  VCTRS_TYPE2_S3_character_bare_ordered,
  VCTRS_TYPE2_S3_character_bare_date,
  VCTRS_TYPE2_S3_character_bare_posixct,
  VCTRS_TYPE2_S3_character_bare_posixlt,
  VCTRS_TYPE2_S3_character_bare_tibble,
  VCTRS_TYPE2_S3_character_unknown,

  VCTRS_TYPE2_S3_raw_bare_factor,
  VCTRS_TYPE2_S3_raw_bare_ordered,
  VCTRS_TYPE2_S3_raw_bare_date,
  VCTRS_TYPE2_S3_raw_bare_posixct,
  VCTRS_TYPE2_S3_raw_bare_posixlt,
  VCTRS_TYPE2_S3_raw_bare_tibble,
  VCTRS_TYPE2_S3_raw_unknown,

  VCTRS_TYPE2_S3_list_bare_factor,
  VCTRS_TYPE2_S3_list_bare_ordered,
  VCTRS_TYPE2_S3_list_bare_date,
  VCTRS_TYPE2_S3_list_bare_posixct,
  VCTRS_TYPE2_S3_list_bare_posixlt,
  VCTRS_TYPE2_S3_list_bare_tibble,
  VCTRS_TYPE2_S3_list_unknown,

  VCTRS_TYPE2_S3_dataframe_bare_factor,
  VCTRS_TYPE2_S3_dataframe_bare_ordered,
  VCTRS_TYPE2_S3_dataframe_bare_date,
  VCTRS_TYPE2_S3_dataframe_bare_posixct,
  VCTRS_TYPE2_S3_dataframe_bare_posixlt,
  VCTRS_TYPE2_S3_dataframe_bare_tibble,
  VCTRS_TYPE2_S3_dataframe_unknown,

  VCTRS_TYPE2_S3_scalar_bare_factor,
  VCTRS_TYPE2_S3_scalar_bare_ordered,
  VCTRS_TYPE2_S3_scalar_bare_date,
  VCTRS_TYPE2_S3_scalar_bare_posixct,
  VCTRS_TYPE2_S3_scalar_bare_posixlt,
  VCTRS_TYPE2_S3_scalar_bare_tibble,
  VCTRS_TYPE2_S3_scalar_unknown,

  VCTRS_TYPE2_S3_bare_factor_bare_factor,
  VCTRS_TYPE2_S3_bare_factor_bare_ordered,
  VCTRS_TYPE2_S3_bare_factor_bare_date,
  VCTRS_TYPE2_S3_bare_factor_bare_posixct,
  VCTRS_TYPE2_S3_bare_factor_bare_posixlt,
  VCTRS_TYPE2_S3_bare_factor_bare_tibble,
  VCTRS_TYPE2_S3_bare_factor_unknown,

  VCTRS_TYPE2_S3_bare_ordered_bare_ordered,
  VCTRS_TYPE2_S3_bare_ordered_bare_date,
  VCTRS_TYPE2_S3_bare_ordered_bare_posixct,
  VCTRS_TYPE2_S3_bare_ordered_bare_posixlt,
  VCTRS_TYPE2_S3_bare_ordered_bare_tibble,
  VCTRS_TYPE2_S3_bare_ordered_unknown,

  VCTRS_TYPE2_S3_bare_date_bare_date,
  VCTRS_TYPE2_S3_bare_date_bare_posixct,
  VCTRS_TYPE2_S3_bare_date_bare_posixlt,
  VCTRS_TYPE2_S3_bare_date_bare_tibble,
  VCTRS_TYPE2_S3_bare_date_unknown,

  VCTRS_TYPE2_S3_bare_posixct_bare_posixct,
  VCTRS_TYPE2_S3_bare_posixct_bare_posixlt,
  VCTRS_TYPE2_S3_bare_posixct_bare_tibble,
  VCTRS_TYPE2_S3_bare_posixct_unknown,

  VCTRS_TYPE2_S3_bare_posixlt_bare_posixlt,
  VCTRS_TYPE2_S3_bare_posixlt_bare_tibble,
  VCTRS_TYPE2_S3_bare_posixlt_unknown,

  VCTRS_TYPE2_S3_bare_tibble_bare_tibble,
  VCTRS_TYPE2_S3_bare_tibble_unknown,

  VCTRS_TYPE2_S3_unknown_unknown
};


enum vctrs_type2_s3 vec_typeof2_s3_impl(r_obj* x,
                                        r_obj* y,
                                        enum vctrs_type type_x,
                                        enum vctrs_type type_y,
                                        int* left);


#endif
