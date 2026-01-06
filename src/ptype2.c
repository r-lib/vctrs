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

  enum s3_fallback s3_fallback = s3_fallback_from_opts(ffi_opts);

  int _;
  return vec_ptype2(
    x,
    y,
    &x_arg,
    &y_arg,
    call,
    s3_fallback,
    &_
  );
}

r_obj* vec_ptype2(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback,
  int* left
) {
  return vec_ptype2_impl(
    x,
    y,
    p_x_arg,
    p_y_arg,
    call,
    s3_fallback,
    left,
    true
  );
}

static
r_obj* vec_ptype2_impl(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback,
  int* left,
  bool first_pass
) {
  const enum vctrs_type x_type = vec_typeof(x);
  const enum vctrs_type y_type = vec_typeof(y);

  if (x_type == VCTRS_TYPE_null) {
    // When `x` and `y` are `NULL`, keep using `x` name (1)
    // When `x` is `NULL` but `y` isn't, switch to `y` name (0)
    *left = y_type == VCTRS_TYPE_null;
    return vec_ptype(y, p_y_arg, call);
  }
  if (y_type == VCTRS_TYPE_null) {
    // When `x` and `y` are `NULL`, keep using `x` name (1)
    // When `y` is `NULL` but `x` isn't, keep using `x` name (1)
    *left = 1;
    return vec_ptype(x, p_x_arg, call);
  }

  if (x_type == VCTRS_TYPE_unspecified) {
    return vec_ptype(y, p_y_arg, call);
  }
  if (y_type == VCTRS_TYPE_unspecified) {
    return vec_ptype(x, p_x_arg, call);
  }

  if (x_type == VCTRS_TYPE_scalar) {
    stop_scalar_type(x, p_x_arg, call);
  }
  if (y_type == VCTRS_TYPE_scalar) {
    stop_scalar_type(y, p_y_arg, call);
  }

  if (x_type != VCTRS_TYPE_s3 && y_type != VCTRS_TYPE_s3) {
    return vec_ptype2_switch_native(
      x,
      y,
      x_type,
      y_type,
      p_x_arg,
      p_y_arg,
      call,
      s3_fallback,
      left
    );
  }

  if (x_type == VCTRS_TYPE_s3 || y_type == VCTRS_TYPE_s3) {
    r_obj* out = KEEP(vec_ptype2_dispatch_native(
      x,
      y,
      x_type,
      y_type,
      p_x_arg,
      p_y_arg,
      call,
      s3_fallback,
      left
    ));

    if (out != r_null) {
      out = vec_shaped_ptype(out, x, y, p_x_arg, p_y_arg);
      FREE(1);
      return out;
    }

    FREE(1);
  }

  // Try native dispatch again with prototypes, in case the prototype
  // is another type. FIXME: Use R-level callback instead.
  if (first_pass) {
    x = KEEP(vec_ptype(x, p_x_arg, call));
    y = KEEP(vec_ptype(y, p_y_arg, call));

    r_obj* out = vec_ptype2_impl(
      x,
      y,
      p_x_arg,
      p_y_arg,
      call,
      s3_fallback,
      left,
      false
    );

    FREE(2);
    return out;
  }

  return vec_ptype2_dispatch_s3(
    x,
    y,
    p_x_arg,
    p_y_arg,
    call,
    s3_fallback
  );
}

static
r_obj* vec_ptype2_switch_native(
  r_obj* x,
  r_obj* y,
  enum vctrs_type x_type,
  enum vctrs_type y_type,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback,
  int* left
) {
  enum vctrs_type2 type2 = vec_typeof2_impl(x_type, y_type, left);

  switch (type2) {
  case VCTRS_TYPE2_null_null:
    return r_null;

  case VCTRS_TYPE2_logical_logical:
    return vec_shaped_ptype(r_globals.empty_lgl, x, y, p_x_arg, p_y_arg);

  case VCTRS_TYPE2_logical_integer:
  case VCTRS_TYPE2_integer_integer:
    return vec_shaped_ptype(r_globals.empty_int, x, y, p_x_arg, p_y_arg);

  case VCTRS_TYPE2_logical_double:
  case VCTRS_TYPE2_integer_double:
  case VCTRS_TYPE2_double_double:
    return vec_shaped_ptype(r_globals.empty_dbl, x, y, p_x_arg, p_y_arg);

  case VCTRS_TYPE2_integer_complex:
  case VCTRS_TYPE2_double_complex:
  case VCTRS_TYPE2_complex_complex:
    return vec_shaped_ptype(r_globals.empty_cpl, x, y, p_x_arg, p_y_arg);

  case VCTRS_TYPE2_character_character:
    return vec_shaped_ptype(r_globals.empty_chr, x, y, p_x_arg, p_y_arg);

  case VCTRS_TYPE2_raw_raw:
    return vec_shaped_ptype(r_globals.empty_raw, x, y, p_x_arg, p_y_arg);

  case VCTRS_TYPE2_list_list:
    return vec_shaped_ptype(r_globals.empty_list, x, y, p_x_arg, p_y_arg);

  case VCTRS_TYPE2_dataframe_dataframe:
    return df_ptype2(
      x,
      y,
      p_x_arg,
      p_y_arg,
      call,
      s3_fallback
    );

  default:
    return vec_ptype2_dispatch_s3(
      x,
      y,
      p_x_arg,
      p_y_arg,
      call,
      s3_fallback
    );
  }
}

struct is_coercible_data {
  r_obj* x;
  r_obj* y;
  struct vctrs_arg* p_x_arg;
  struct vctrs_arg* p_y_arg;
  struct r_lazy call;
  enum s3_fallback s3_fallback;
  r_obj* out;
};

static
void vec_is_coercible_cb(void* data_) {
  struct is_coercible_data* data = (struct is_coercible_data*) data_;
  int _;
  data->out = vec_ptype2(
    data->x,
    data->y,
    data->p_x_arg,
    data->p_y_arg,
    data->call,
    data->s3_fallback,
    &_
  );
}

static
void vec_is_coercible_e(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback,
  ERR* err
) {
  struct is_coercible_data data = {
    .x = x,
    .y = y,
    .p_x_arg = p_x_arg,
    .p_y_arg = p_y_arg,
    .call = call,
    .s3_fallback = s3_fallback,
    .out = r_null
  };

  *err = r_try_catch(&vec_is_coercible_cb,
                     &data,
                     syms_vctrs_error_incompatible_type,
                     NULL,
                     NULL);
}

bool vec_is_coercible(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback
) {
  ERR err = NULL;
  vec_is_coercible_e(
    x,
    y,
    p_x_arg,
    p_y_arg,
    call,
    s3_fallback,
    &err
  );
  return !err;
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

  const enum s3_fallback s3_fallback = s3_fallback_from_opts(opts);

  return r_lgl(vec_is_coercible(
    x,
    y,
    &x_arg,
    &y_arg,
    call,
    s3_fallback
  ));
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

  int _;
  return vec_ptype2(
    x,
    y,
    &x_arg,
    &y_arg,
    call,
    S3_FALLBACK_false,
    &_
  );
}

// Order on R side is important
enum s3_fallback s3_fallback_from_opts(r_obj* opts) {
  return (enum s3_fallback) r_int_get(r_list_get(opts, 0), 0);
}


void vctrs_init_ptype2(r_obj* ns) { }
