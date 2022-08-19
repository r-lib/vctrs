#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/ptype2-decl.h"

// [[ register() ]]
r_obj* ffi_ptype2_opts(r_obj* x,
                       r_obj* y,
                       r_obj* ffi_opts,
                       r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  struct r_lazy call = { .x = r_syms.call, .env = frame, };

  struct ptype2_opts opts = new_ptype2_opts(x,
                                            y,
                                            &x_arg,
                                            &y_arg,
                                            call,
                                            ffi_opts);

  int _left;
  return vec_ptype2_opts(&opts, &_left);
}

r_obj* vec_ptype2_opts_impl(const struct ptype2_opts* opts,
                            int* left,
                            bool first_pass) {
  r_obj* x = opts->x;
  r_obj* y = opts->y;
  struct vctrs_arg* x_arg = opts->p_x_arg;
  struct vctrs_arg* y_arg = opts->p_y_arg;

  enum vctrs_type x_type = vec_typeof(x);
  enum vctrs_type y_type = vec_typeof(y);

  if (x_type == vctrs_type_null) {
    *left = y == r_null;
    return vec_ptype2_from_unspecified(opts, x_type, y, y_arg);
  }
  if (y_type == vctrs_type_null) {
    *left = x == r_null;
    return vec_ptype2_from_unspecified(opts, x_type, x, x_arg);
  }

  if (x_type == vctrs_type_unspecified) {
    return vec_ptype2_from_unspecified(opts, y_type, y, y_arg);
  }
  if (y_type == vctrs_type_unspecified) {
    return vec_ptype2_from_unspecified(opts, x_type, x, x_arg);
  }

  if (x_type == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg, opts->call);
  }
  if (y_type == vctrs_type_scalar) {
    stop_scalar_type(y, y_arg, opts->call);
  }

  if (x_type != vctrs_type_s3 && y_type != vctrs_type_s3) {
    return vec_ptype2_switch_native(opts, x_type, y_type, left);
  }

  if (x_type == vctrs_type_s3 || y_type == vctrs_type_s3) {
    r_obj* out = vec_ptype2_dispatch_native(opts, x_type, y_type, left);
    if (out != r_null) {
      return out;
    }
  }

  // Try native dispatch again with prototypes, in case the prototype
  // is another type. FIXME: Use R-level callback instead.
  if (first_pass) {
    struct ptype2_opts mut_opts = *opts;
    mut_opts.x = KEEP(vec_ptype(x, x_arg, opts->call));
    mut_opts.y = KEEP(vec_ptype(y, y_arg, opts->call));

    r_obj* out = vec_ptype2_opts_impl(&mut_opts, left, false);

    FREE(2);
    return out;
  }

  return vec_ptype2_dispatch_s3(opts);
}

r_obj* vec_ptype2_opts(const struct ptype2_opts* opts,
                     int* left) {
  return vec_ptype2_opts_impl(opts, left, true);
}

static
r_obj* vec_ptype2_switch_native(const struct ptype2_opts* opts,
                                enum vctrs_type x_type,
                                enum vctrs_type y_type,
                                int* left) {
  r_obj* x = opts->x;
  r_obj* y = opts->y;
  struct vctrs_arg* x_arg = opts->p_x_arg;
  struct vctrs_arg* y_arg = opts->p_y_arg;

  enum vctrs_type2 type2 = vec_typeof2_impl(x_type, y_type, left);

  switch (type2) {
  case vctrs_type2_null_null:
    return r_null;

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
r_obj* vec_ptype2_from_unspecified(const struct ptype2_opts* opts,
                                   enum vctrs_type other_type,
                                   r_obj* other,
                                   struct vctrs_arg* other_arg) {
  if (other_type == vctrs_type_unspecified || other_type == vctrs_type_null) {
    return vec_ptype(other, other_arg, opts->call);
  }

  if (opts->fallback.s3) {
    const struct ptype2_opts self_self_opts = (const struct ptype2_opts) {
      .x = other,
      .y = other,
      .p_x_arg = other_arg,
      .p_y_arg = other_arg,
      .fallback = opts->fallback
    };
    int _left = 0;
    return vec_ptype2_opts(&self_self_opts, &_left);
  }

  return vec_ptype(other, other_arg, opts->call);
}


struct is_coercible_data {
  const struct ptype2_opts* opts;
  int* dir;
  r_obj* out;
};

static
void vec_is_coercible_cb(void* data_) {
  struct is_coercible_data* data = (struct is_coercible_data*) data_;
  data->out = vec_ptype2_opts(data->opts, data->dir);
}

static
void vec_is_coercible_e(const struct ptype2_opts* opts,
                        int* dir,
                        ERR* err) {
  struct is_coercible_data data = {
    .opts = opts,
    .dir = dir,
    .out = r_null
  };

  *err = r_try_catch(&vec_is_coercible_cb,
                     &data,
                     syms_vctrs_error_incompatible_type,
                     NULL,
                     NULL);
}

bool vec_is_coercible(const struct ptype2_opts* opts,
                      int* dir) {
  ERR err = NULL;
  vec_is_coercible_e(opts, dir, &err);
  return !err;
}

r_obj* vec_ptype2_e(const struct ptype2_opts* opts,
                    int* dir,
                    ERR* err) {
  struct is_coercible_data data = {
    .opts = opts,
    .dir = dir,
    .out = r_null
  };

  *err = r_try_catch(&vec_is_coercible_cb,
                     &data,
                     syms_vctrs_error_incompatible_type,
                     NULL,
                     NULL);

  return data.out;
}

// [[ register() ]]
r_obj* ffi_is_coercible(r_obj* x,
                        r_obj* y,
                        r_obj* opts,
                        r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  struct r_lazy call = { .x = syms_call, .env = frame };

  const struct ptype2_opts c_opts = new_ptype2_opts(x,
                                                    y,
                                                    &x_arg,
                                                    &y_arg,
                                                    call,
                                                    opts);

  int dir = 0;
  return r_lgl(vec_is_coercible(&c_opts, &dir));
}


// [[ register() ]]
r_obj* ffi_ptype2(r_obj* x,
                  r_obj* y,
                  r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  struct r_lazy call = { .x = syms_call, .env = frame };

  int _left;
  return vec_ptype2(x, y, &x_arg, &y_arg, &_left, call);
}

struct ptype2_opts new_ptype2_opts(r_obj* x,
                                   r_obj* y,
                                   struct vctrs_arg* p_x_arg,
                                   struct vctrs_arg* p_y_arg,
                                   struct r_lazy call,
                                   r_obj* opts) {
  return (struct ptype2_opts) {
    .x = x,
    .y = y,
    .p_x_arg = p_x_arg,
    .p_y_arg = p_y_arg,
    .call = call,
    .fallback = new_fallback_opts(opts)
  };
}

struct fallback_opts new_fallback_opts(r_obj* opts) {
  return (struct fallback_opts) {
    .df = r_int_get(r_list_get(opts, 0), 0),
    .s3 = r_int_get(r_list_get(opts, 1), 0)
  };
}


void vctrs_init_ptype2(r_obj* ns) { }
