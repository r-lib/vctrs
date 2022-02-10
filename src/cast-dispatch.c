#include "vctrs.h"
#include "type-factor.h"
#include "type-tibble.h"
#include "utils.h"

r_obj* vec_cast_dispatch_native(const struct cast_opts* opts,
                                enum vctrs_type x_type,
                                enum vctrs_type to_type,
                                bool* lossy) {
  r_obj* x = opts->x;
  r_obj* to = opts->to;
  struct vctrs_arg* x_arg = opts->x_arg;
  struct vctrs_arg* to_arg = opts->to_arg;

  int dir = 0;
  enum vctrs_type2_s3 type2_s3 = vec_typeof2_s3_impl(x, to, x_type, to_type, &dir);

  switch (type2_s3) {

  case vctrs_type2_s3_character_bare_factor:
    if (dir == 0) {
      return chr_as_factor(x, to, lossy, to_arg);
    } else {
      return fct_as_character(x, x_arg);
    }

  case vctrs_type2_s3_character_bare_ordered:
    if (dir == 0) {
      return chr_as_ordered(x, to, lossy, to_arg);
    } else {
      return ord_as_character(x, x_arg);
    }

  case vctrs_type2_s3_bare_factor_bare_factor:
    return fct_as_factor(x, to, lossy, x_arg, to_arg);

  case vctrs_type2_s3_bare_ordered_bare_ordered:
    return ord_as_ordered(opts);

  case vctrs_type2_s3_bare_date_bare_posixct:
    if (dir == 0) {
      return date_as_posixct(x, to);
    } else {
      return posixct_as_date(x, lossy);
    }

  case vctrs_type2_s3_bare_date_bare_posixlt:
    if (dir == 0) {
      return date_as_posixlt(x, to);
    } else {
      return posixlt_as_date(x, lossy);
    }

  case vctrs_type2_s3_bare_posixct_bare_posixlt:
    if (dir == 0) {
      return posixct_as_posixlt(x, to);
    } else {
      return posixlt_as_posixct(x, to);
    }

  case vctrs_type2_s3_bare_date_bare_date:
    return date_as_date(x);

  case vctrs_type2_s3_bare_posixct_bare_posixct:
    return posixct_as_posixct(x, to);

  case vctrs_type2_s3_bare_posixlt_bare_posixlt:
    return posixlt_as_posixlt(x, to);

  case vctrs_type2_s3_dataframe_bare_tibble:
    if (dir == 0) {
      return tib_cast(opts);
    } else {
      return df_cast_opts(opts);
    }

  case vctrs_type2_s3_bare_tibble_bare_tibble:
    return tib_cast(opts);

  default:
    return r_null;
  }
}

// [[ register() ]]
r_obj* ffi_cast_dispatch_native(r_obj* x,
                                r_obj* to,
                                r_obj* fallback_opts,
                                r_obj* x_arg,
                                r_obj* to_arg,
                                r_obj* frame) {
  struct vctrs_arg c_x_arg = vec_as_arg(x_arg);
  struct vctrs_arg c_to_arg = vec_as_arg(to_arg);
  struct r_lazy call = { .x = syms_call, .env = frame };

  struct cast_opts c_opts = new_cast_opts(x,
                                          to,
                                          &c_x_arg,
                                          &c_to_arg,
                                          call,
                                          fallback_opts);

  bool lossy = false;
  r_obj* out = vec_cast_dispatch_native(&c_opts, vec_typeof(x), vec_typeof(to), &lossy);

  if (lossy || out == r_null) {
    return vec_cast_default(x,
                            to,
                            x_arg,
                            to_arg,
                            c_opts.call,
                            &c_opts.fallback);
  } else {
    return out;
  }
}
