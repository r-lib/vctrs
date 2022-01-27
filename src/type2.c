#include <rlang.h>
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

SEXP vec_ptype2_opts_impl(const struct ptype2_opts* opts,
                          int* left,
                          bool first_pass) {
  SEXP x = opts->x;
  SEXP y = opts->y;
  struct vctrs_arg* x_arg = opts->x_arg;
  struct vctrs_arg* y_arg = opts->y_arg;

  enum vctrs_type x_type = vec_typeof(x);
  enum vctrs_type y_type = vec_typeof(y);

  if (x_type == vctrs_type_null) {
    *left = y == R_NilValue;
    return vec_ptype2_from_unspecified(opts, x_type, y, y_arg);
  }
  if (y_type == vctrs_type_null) {
    *left = x == R_NilValue;
    return vec_ptype2_from_unspecified(opts, x_type, x, x_arg);
  }

  if (x_type == vctrs_type_unspecified) {
    return vec_ptype2_from_unspecified(opts, y_type, y, y_arg);
  }
  if (y_type == vctrs_type_unspecified) {
    return vec_ptype2_from_unspecified(opts, x_type, x, x_arg);
  }

  if (x_type == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg);
  }
  if (y_type == vctrs_type_scalar) {
    stop_scalar_type(y, y_arg);
  }

  if (x_type != vctrs_type_s3 && y_type != vctrs_type_s3) {
    return vec_ptype2_switch_native(opts, x_type, y_type, left);
  }

  if (x_type == vctrs_type_s3 || y_type == vctrs_type_s3) {
    SEXP out = vec_ptype2_dispatch_native(opts, x_type, y_type, left);
    if (out != R_NilValue) {
      return out;
    }
  }

  // Try native dispatch again with prototypes, in case the prototype
  // is another type. FIXME: Use R-level callback instead.
  if (first_pass) {
    struct ptype2_opts mut_opts = *opts;
    mut_opts.x = PROTECT(vec_ptype(x, x_arg));
    mut_opts.y = PROTECT(vec_ptype(y, y_arg));

    SEXP out = vec_ptype2_opts_impl(&mut_opts, left, false);

    UNPROTECT(2);
    return out;
  }

  return vec_ptype2_dispatch_s3(opts);
}

// [[ include("ptype2.h") ]]
SEXP vec_ptype2_opts(const struct ptype2_opts* opts,
                     int* left) {
  return vec_ptype2_opts_impl(opts, left, true);
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

/**
 * Return non-unspecified type.
 *
 * This is normally the `vec_ptype()` of the other input, but if the
 * common class fallback is enabled we return the `vec_ptype2()` of
 * this input with itself. This way we may return a fallback sentinel which can be
 * treated specially, for instance in `vec_c(NA, x, NA)`.
 */
SEXP vec_ptype2_from_unspecified(const struct ptype2_opts* opts,
                                 enum vctrs_type other_type,
                                 SEXP other,
                                 struct vctrs_arg* other_arg) {
  if (other_type == vctrs_type_unspecified || other_type == vctrs_type_null) {
    return vec_ptype(other, other_arg);
  }

  if (opts->fallback.s3) {
    const struct ptype2_opts self_self_opts = (const struct ptype2_opts) {
      .x = other,
      .y = other,
      .x_arg = other_arg,
      .y_arg = other_arg,
      .fallback = opts->fallback
    };
    int _left = 0;
    return vec_ptype2_opts(&self_self_opts, &_left);
  }

  return vec_ptype(other, other_arg);
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
                        SEXP opts,
                        SEXP x_arg,
                        SEXP y_arg) {
  struct vctrs_arg c_x_arg = vec_as_arg(x_arg);
  struct vctrs_arg c_y_arg = vec_as_arg(y_arg);

  const struct ptype2_opts c_opts = new_ptype2_opts(x, y, &c_x_arg, &c_y_arg, opts);

  int dir = 0;
  return r_lgl(vec_is_coercible(&c_opts, &dir));
}


// [[ register() ]]
r_obj* ffi_ptype2(r_obj* x,
                  r_obj* y,
                  r_obj* frame) {
  struct arg_data_lazy x_arg_ = new_lazy_arg_data(frame, "x_arg");
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);
  struct arg_data_lazy y_arg_ = new_lazy_arg_data(frame, "y_arg");
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  struct r_lazy call = { .x = syms_call, .env = frame };

  int _left;
  return vec_ptype2(x, y, &x_arg, &y_arg, &_left, call);
}

// [[ include("ptype2.h") ]]
struct fallback_opts new_fallback_opts(SEXP opts) {
  return (struct fallback_opts) {
    .df = r_int_get(r_list_get(opts, 0), 0),
    .s3 = r_int_get(r_list_get(opts, 1), 0)
  };
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
    .fallback = new_fallback_opts(opts)
  };
}

static SEXP r_fallback_opts_template = NULL;

// [[ include("ptype2.h") ]]
SEXP new_fallback_r_opts(const struct ptype2_opts* opts) {
  SEXP r_opts = PROTECT(r_copy(r_fallback_opts_template));

  r_int_poke(r_list_get(r_opts, 0), 0, opts->fallback.df);
  r_int_poke(r_list_get(r_opts, 1), 0, opts->fallback.s3);

  UNPROTECT(1);
  return r_opts;
}


void vctrs_init_ptype2(SEXP ns) {
  r_fallback_opts_template = r_parse_eval("fallback_opts()", ns);
  R_PreserveObject(r_fallback_opts_template);
}
