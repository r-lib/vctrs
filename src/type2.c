#include "vctrs.h"
#include "ptype2.h"
#include "type-data-frame.h"
#include "utils.h"
#include "shape.h"

static
SEXP vec_ptype2_switch_native(const struct ptype2_opts* opts,
                              enum vctrs_type x_type,
                              enum vctrs_type y_type,
                              int* left);

// [[ register() ]]
SEXP vctrs_ptype2_opts(SEXP x,
                       SEXP y,
                       SEXP opts,
                       SEXP x_arg,
                       SEXP y_arg) {
  struct vctrs_arg c_x_arg = vec_as_arg(x_arg);
  struct vctrs_arg c_y_arg = vec_as_arg(y_arg);

  const struct ptype2_opts c_opts = new_ptype2_opts(x, y, &c_x_arg, &c_y_arg, opts);

  int _left;
  return vec_ptype2_opts(&c_opts, &_left);
}

SEXP vec_ptype2_opts(const struct ptype2_opts* opts,
                     int* left) {
  SEXP x = opts->x;
  SEXP y = opts->y;
  struct vctrs_arg* x_arg = opts->x_arg;
  struct vctrs_arg* y_arg = opts->y_arg;

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
    return vec_ptype2_dispatch(opts, type_x, type_y, left);
  } else {
    return vec_ptype2_switch_native(opts, type_x, type_y, left);
  }
}

static
SEXP vec_ptype2_switch_native(const struct ptype2_opts* opts,
                              enum vctrs_type x_type,
                              enum vctrs_type y_type,
                              int* left) {
  SEXP x = opts->x;
  SEXP y = opts->y;
  struct vctrs_arg* x_arg = opts->x_arg;
  struct vctrs_arg* y_arg = opts->y_arg;

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
    return df_ptype2(opts);

  default:
    return vec_ptype2_dispatch_s3(opts);
  }
}


struct is_coercible_data {
  const struct ptype2_opts* opts;
  int* dir;
};

static
void vec_is_coercible_cb(void* data_) {
  struct is_coercible_data* data = (struct is_coercible_data*) data_;
  vec_ptype2_opts(data->opts, data->dir);
}

static
void vec_is_coercible_e(const struct ptype2_opts* opts,
                        int* dir,
                        ERR* err) {
  struct is_coercible_data data = {
    .opts = opts,
    .dir = dir,
  };

  *err = r_try_catch(&vec_is_coercible_cb,
                     &data,
                     syms_vctrs_error_incompatible_type,
                     NULL,
                     NULL);
}

// [[ include("ptype2.h") ]]
bool vec_is_coercible(const struct ptype2_opts* opts,
                      int* dir) {
  ERR err = NULL;
  vec_is_coercible_e(opts, dir, &err);
  return !err;
}

// [[ register() ]]
SEXP vctrs_is_coercible(SEXP x,
                        SEXP y,
                        SEXP x_arg_,
                        SEXP y_arg_,
                        SEXP df_fallback_,
                        SEXP s3_fallback_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg y_arg = vec_as_arg(y_arg_);;
  enum df_fallback df_fallback = r_int_get(df_fallback_, 0);
  enum s3_fallback s3_fallback = r_int_get(s3_fallback_, 0);

  const struct ptype2_opts opts = {
    .x = x,
    .y = y,
    .x_arg = &x_arg,
    .y_arg = &y_arg,
    .df_fallback = df_fallback,
    .s3_fallback = s3_fallback
  };

  int dir = 0;
  return r_lgl(vec_is_coercible(&opts, &dir));
}


// [[ register() ]]
SEXP vctrs_ptype2(SEXP x, SEXP y, SEXP x_arg, SEXP y_arg) {
  struct vctrs_arg x_arg_ = vec_as_arg(x_arg);
  struct vctrs_arg y_arg_ = vec_as_arg(y_arg);

  int _left;
  return vec_ptype2(x, y, &x_arg_, &y_arg_, &_left);
}

// [[ include("ptype2.h") ]]
struct ptype2_opts new_ptype2_opts(SEXP x,
                                   SEXP y,
                                   struct vctrs_arg* x_arg,
                                   struct vctrs_arg* y_arg,
                                   SEXP opts) {
  return (struct ptype2_opts) {
    .x = x,
    .y = y,
    .x_arg = x_arg,
    .y_arg = y_arg,
    .df_fallback = r_int_get(r_list_get(opts, 0), 0),
    .s3_fallback = r_int_get(r_list_get(opts, 1), 0)
  };
}
