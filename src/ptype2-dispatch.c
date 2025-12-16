#include "vctrs.h"
#include "type-data-frame.h"
#include "type-factor.h"
#include "type-tibble.h"
#include "decl/ptype2-dispatch-decl.h"

r_obj* vec_ptype2_dispatch_native(
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
  enum vctrs_type2_s3 type2_s3 = vec_typeof2_s3_impl(x, y, x_type, y_type, left);

  switch (type2_s3) {
  case VCTRS_TYPE2_S3_character_bare_factor:
  case VCTRS_TYPE2_S3_character_bare_ordered:
    return r_globals.empty_chr;

  case VCTRS_TYPE2_S3_bare_factor_bare_factor:
    return fct_ptype2(
      x,
      y,
      p_x_arg,
      p_y_arg
    );

  case VCTRS_TYPE2_S3_bare_ordered_bare_ordered:
    return ord_ptype2(
      x,
      y,
      p_x_arg,
      p_y_arg,
      call,
      s3_fallback
    );

  case VCTRS_TYPE2_S3_bare_date_bare_date:
    return vctrs_shared_empty_date;

  case VCTRS_TYPE2_S3_bare_date_bare_posixct:
  case VCTRS_TYPE2_S3_bare_date_bare_posixlt:
    return date_datetime_ptype2(x, y);

  case VCTRS_TYPE2_S3_bare_posixct_bare_posixct:
  case VCTRS_TYPE2_S3_bare_posixct_bare_posixlt:
  case VCTRS_TYPE2_S3_bare_posixlt_bare_posixlt:
    return datetime_datetime_ptype2(x, y);

  case VCTRS_TYPE2_S3_dataframe_bare_tibble:
  case VCTRS_TYPE2_S3_bare_tibble_bare_tibble:
    return tib_ptype2(
      x,
      y,
      p_x_arg,
      p_y_arg,
      call,
      s3_fallback
    );

  default:
    return r_null;
  }
}

// @param from_dispatch Used to implement special behaviour when
//   `vec_default_ptype2()` is invoked directly from the dispatch
//   mechanism as opposed from a method.

static inline
r_obj* vec_ptype2_default_full(r_obj* x,
                               r_obj* y,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* y_arg,
                               struct r_lazy call,
                               enum s3_fallback s3_fallback,
                               bool from_dispatch) {
  r_obj* ffi_s3_fallback = KEEP(r_int(s3_fallback));
  r_obj* ffi_x_arg = KEEP(vctrs_arg(x_arg));
  r_obj* ffi_y_arg = KEEP(vctrs_arg(y_arg));
  r_obj* ffi_call = KEEP(r_lazy_eval(call));

  r_obj* out = vctrs_eval_mask7(syms_vec_ptype2_default,
                                syms_x, x,
                                syms_y, y,
                                syms_x_arg, ffi_x_arg,
                                syms_y_arg, ffi_y_arg,
                                syms_call, ffi_call,
                                syms_from_dispatch, r_lgl(from_dispatch),
                                syms_s3_fallback, ffi_s3_fallback);

  FREE(4);
  return out;
}

r_obj* vec_ptype2_default(r_obj* x,
                          r_obj* y,
                          struct vctrs_arg* x_arg,
                          struct vctrs_arg* y_arg,
                          struct r_lazy call,
                          enum s3_fallback s3_fallback) {
  return vec_ptype2_default_full(x, y, x_arg, y_arg, call, s3_fallback, false);
}

r_obj* vec_ptype2_dispatch_s3(const struct ptype2_opts* opts) {
  r_obj* x = KEEP(vec_ptype(opts->x, opts->p_x_arg, opts->call));
  r_obj* y = KEEP(vec_ptype(opts->y, opts->p_y_arg, opts->call));

  r_obj* method_sym = r_null;
  r_obj* method = s3_find_method_xy("vec_ptype2", x, y, vctrs_method_table, &method_sym);

  // Compatibility with legacy double dispatch mechanism
  if (method == r_null) {
    r_obj* x_method_sym = r_null;
    r_obj* x_method = KEEP(s3_find_method2("vec_ptype2",
                                           x,
                                           vctrs_method_table,
                                           &x_method_sym));

    if (x_method != r_null) {
      // Only `x_method`s contained within a package will
      // have an S3 methods table to look in
      r_obj* x_table = s3_get_table(r_fn_env(x_method));

      if (x_table != r_null) {
        const char* x_method_str = r_sym_c_string(x_method_sym);

        method = s3_find_method2(
          x_method_str,
          y,
          x_table,
          &method_sym
        );
      }
    }

    FREE(1);
  }

  KEEP(method);

  if (method == r_null) {
    r_obj* out = vec_ptype2_default_full(x,
                                         y,
                                         opts->p_x_arg,
                                         opts->p_y_arg,
                                         opts->call,
                                         opts->s3_fallback,
                                         true);
    FREE(3);
    return out;
  }

  r_obj* ffi_x_arg = KEEP(vctrs_arg(opts->p_x_arg));
  r_obj* ffi_y_arg = KEEP(vctrs_arg(opts->p_y_arg));

  r_obj* out = vec_invoke_coerce_method(method_sym, method,
                                        syms_x, x,
                                        syms_y, y,
                                        syms_x_arg, ffi_x_arg,
                                        syms_y_arg, ffi_y_arg,
                                        opts->call,
                                        opts->s3_fallback);

  FREE(5);
  return out;
}

r_obj* vec_invoke_coerce_method(r_obj* method_sym, r_obj* method,
                                r_obj* x_sym, r_obj* x,
                                r_obj* y_sym, r_obj* y,
                                r_obj* x_arg_sym, r_obj* x_arg,
                                r_obj* y_arg_sym, r_obj* y_arg,
                                struct r_lazy lazy_call,
                                enum s3_fallback s3_fallback) {
  r_obj* call = KEEP(r_lazy_eval(lazy_call));

  if (s3_fallback != S3_FALLBACK_false) {
    r_obj* ffi_s3_fallback = KEEP(r_int(s3_fallback));

    r_obj* out = vctrs_dispatch6(method_sym, method,
                                 x_sym, x,
                                 y_sym, y,
                                 x_arg_sym, x_arg,
                                 y_arg_sym, y_arg,
                                 syms_call, call,
                                 syms_s3_fallback, ffi_s3_fallback);
    FREE(2);
    return out;
  } else {
    r_obj* out = vctrs_dispatch5(method_sym, method,
                                 x_sym, x,
                                 y_sym, y,
                                 x_arg_sym, x_arg,
                                 y_arg_sym, y_arg,
                                 syms_call, call);
    FREE(1);
    return out;
  }
}

// [[ register() ]]
r_obj* ffi_ptype2_dispatch_native(r_obj* x,
                                  r_obj* y,
                                  r_obj* opts,
                                  r_obj* frame) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  struct r_lazy call = { .x = syms_call, .env = frame };

  const enum s3_fallback s3_fallback = s3_fallback_from_opts(opts);

  int _;
  r_obj* out = vec_ptype2_dispatch_native(
    x,
    y,
    vec_typeof(x),
    vec_typeof(y),
    &x_arg,
    &y_arg,
    call,
    s3_fallback,
    &_
  );

  if (out == r_null) {
    out = vec_ptype2_default_full(
      x,
      y,
      &x_arg,
      &y_arg,
      call,
      s3_fallback,
      true
    );
    return out;
  } else {
    return out;
  }
}


void vctrs_init_ptype2_dispatch(r_obj* ns) {
  syms_vec_ptype2_default = r_sym("vec_default_ptype2");
}

static
r_obj* syms_vec_ptype2_default = NULL;
