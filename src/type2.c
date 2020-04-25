#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"
#include "shape.h"

static SEXP vec_ptype2_switch_native(SEXP x,
                                     SEXP y,
                                     enum vctrs_type x_type,
                                     enum vctrs_type y_type,
                                     struct vctrs_arg* x_arg,
                                     struct vctrs_arg* y_arg,
                                     int* left,
                                     bool df_fallback);

// [[ register() ]]
SEXP vctrs_ptype2_params(SEXP x,
                         SEXP y,
                         SEXP x_arg,
                         SEXP y_arg,
                         SEXP df_fallback) {
  struct vctrs_arg x_arg_ = vec_as_arg(x_arg);
  struct vctrs_arg y_arg_ = vec_as_arg(y_arg);
  bool df_fallback_ = LOGICAL(df_fallback)[0];
  int _left;
  return vec_ptype2_params(x, y, df_fallback_, &y_arg_, &x_arg_, &_left);
}

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_params(SEXP x,
                       SEXP y,
                       bool df_fallback,
                       struct vctrs_arg* x_arg,
                       struct vctrs_arg* y_arg,
                       int* left) {
  if (x == R_NilValue) {
    *left = y == R_NilValue;
    return vec_ptype(y, y_arg);
  }
  if (y == R_NilValue) {
    *left = x == R_NilValue;
    return vec_ptype(x, x_arg);
  }

  enum vctrs_type type_x = vec_typeof(x);
  enum vctrs_type type_y = vec_typeof(y);

  if (type_x == vctrs_type_unspecified) {
    return vec_ptype(y, y_arg);
  }
  if (type_y == vctrs_type_unspecified) {
    return vec_ptype(x, x_arg);
  }

  if (type_x == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg);
  }
  if (type_y == vctrs_type_scalar) {
    stop_scalar_type(y, y_arg);
  }

  if (type_x == vctrs_type_s3 || type_y == vctrs_type_s3) {
    return vec_ptype2_dispatch(x, y, type_x, type_y, x_arg, y_arg, left, df_fallback);
  } else {
    return vec_ptype2_switch_native(x, y, type_x, type_y, x_arg, y_arg, left, df_fallback);
  }
}

static SEXP vec_ptype2_switch_native(SEXP x,
                                     SEXP y,
                                     enum vctrs_type x_type,
                                     enum vctrs_type y_type,
                                     struct vctrs_arg* x_arg,
                                     struct vctrs_arg* y_arg,
                                     int* left,
                                     bool df_fallback) {
  enum vctrs_type2 type2 = vec_typeof2_impl(x_type, y_type, left);

  switch (type2) {
  case vctrs_type2_null_null:
    return R_NilValue;

  case vctrs_type2_logical_logical:
    return vec_shaped_ptype(vctrs_shared_empty_lgl, x, y, x_arg, y_arg);

  case vctrs_type2_logical_integer:
  case vctrs_type2_integer_integer:
    return vec_shaped_ptype(vctrs_shared_empty_int, x, y, x_arg, y_arg);

  case vctrs_type2_logical_double:
  case vctrs_type2_integer_double:
  case vctrs_type2_double_double:
    return vec_shaped_ptype(vctrs_shared_empty_dbl, x, y, x_arg, y_arg);

  case vctrs_type2_integer_complex:
  case vctrs_type2_double_complex:
  case vctrs_type2_complex_complex:
    return vec_shaped_ptype(vctrs_shared_empty_cpl, x, y, x_arg, y_arg);

  case vctrs_type2_character_character:
    return vec_shaped_ptype(vctrs_shared_empty_chr, x, y, x_arg, y_arg);

  case vctrs_type2_raw_raw:
    return vec_shaped_ptype(vctrs_shared_empty_raw, x, y, x_arg, y_arg);

  case vctrs_type2_list_list:
    return vec_shaped_ptype(vctrs_shared_empty_list, x, y, x_arg, y_arg);

  case vctrs_type2_dataframe_dataframe:
    return df_ptype2_params(x, y, x_arg, y_arg, df_fallback);

  default:
    return vec_ptype2_dispatch_s3(x, y, x_arg, y_arg, df_fallback);
  }
}

// [[ register() ]]
SEXP vctrs_ptype2(SEXP x, SEXP y, SEXP x_arg, SEXP y_arg) {
  struct vctrs_arg x_arg_ = vec_as_arg(x_arg);
  struct vctrs_arg y_arg_ = vec_as_arg(y_arg);

  int _left;
  return vec_ptype2(x, y, &x_arg_, &y_arg_, &_left);
}
