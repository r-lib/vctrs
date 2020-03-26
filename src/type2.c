#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

static SEXP vec_ptype2_switch_native(SEXP x,
                                     SEXP y,
                                     enum vctrs_type x_type,
                                     enum vctrs_type y_type,
                                     struct vctrs_arg* x_arg,
                                     struct vctrs_arg* y_arg,
                                     int* left);

// [[ include("vctrs.h") ]]
SEXP vec_ptype2(SEXP x, SEXP y,
               struct vctrs_arg* x_arg,
               struct vctrs_arg* y_arg,
               int* left) {
  if (x == R_NilValue) {
    if (!vec_is_partial(y)) {
      vec_assert(y, y_arg);
    }
    *left = y == R_NilValue;
    return vec_type(y);
  }
  if (y == R_NilValue) {
    if (!vec_is_partial(x)) {
      vec_assert(x, x_arg);
    }
    *left = x == R_NilValue;
    return vec_type(x);
  }

  enum vctrs_type type_x = vec_typeof(x);
  enum vctrs_type type_y = vec_typeof(y);

  if (type_x == vctrs_type_unspecified) {
    return vec_type(y);
  }
  if (type_y == vctrs_type_unspecified) {
    return vec_type(x);
  }

  if (has_dim(x) || has_dim(y)) {
    return vec_ptype2_dispatch_s3(x, y, x_arg, y_arg);
  }

  if (type_x == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg);
  }
  if (type_y == vctrs_type_scalar) {
    stop_scalar_type(y, y_arg);
  }

  if (type_x == vctrs_type_s3 || type_y == vctrs_type_s3) {
    return vec_ptype2_dispatch(x, y, type_x, type_y, x_arg, y_arg, left);
  } else {
    return vec_ptype2_switch_native(x, y, type_x, type_y, x_arg, y_arg, left);
  }
}

static SEXP vec_ptype2_switch_native(SEXP x,
                                     SEXP y,
                                     enum vctrs_type x_type,
                                     enum vctrs_type y_type,
                                     struct vctrs_arg* x_arg,
                                     struct vctrs_arg* y_arg,
                                     int* left) {
  enum vctrs_type2 type2 = vec_typeof2_impl(x_type, y_type, left);

  switch (type2) {
  case vctrs_type2_null_null:
    return R_NilValue;

  case vctrs_type2_logical_logical:
    return vctrs_shared_empty_lgl;

  case vctrs_type2_logical_integer:
  case vctrs_type2_integer_integer:
    return vctrs_shared_empty_int;

  case vctrs_type2_logical_double:
  case vctrs_type2_integer_double:
  case vctrs_type2_double_double:
    return vctrs_shared_empty_dbl;

  case vctrs_type2_character_character:
    return vctrs_shared_empty_chr;

  case vctrs_type2_raw_raw:
    return vctrs_shared_empty_raw;

  case vctrs_type2_list_list:
    return vctrs_shared_empty_list;

  case vctrs_type2_dataframe_dataframe:
    return df_ptype2(x, y, x_arg, y_arg);

  default:
    return vec_ptype2_dispatch_s3(x, y, x_arg, y_arg);
  }
}

// [[ register() ]]
SEXP vctrs_ptype2(SEXP x, SEXP y, SEXP x_arg, SEXP y_arg) {
  struct vctrs_arg x_arg_ = vec_as_arg(x_arg);
  struct vctrs_arg y_arg_ = vec_as_arg(y_arg);

  int _left;
  return vec_ptype2(x, y, &x_arg_, &y_arg_, &_left);
}
