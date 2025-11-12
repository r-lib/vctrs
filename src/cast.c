#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/cast-decl.h"

// [[ register() ]]
r_obj* ffi_cast(r_obj* x,
                r_obj* to,
                r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy to_arg_ = { .x = syms.to_arg, .env = frame };
  struct vctrs_arg to_arg = new_lazy_arg(&to_arg_);

  struct r_lazy call = { .x = syms_call, .env = frame };

  return vec_cast(x, to, &x_arg, &to_arg, call);
}

r_obj* vec_cast_opts(const struct cast_opts* opts) {
  r_obj* x = opts->x;
  r_obj* to = opts->to;
  struct vctrs_arg* x_arg = opts->p_x_arg;
  struct vctrs_arg* to_arg = opts->p_to_arg;

  if (x == r_null) {
    // Allow both `vec_cast(NULL, <vector>)` and `vec_cast(NULL, NULL)`
    obj_check_vector(to, VCTRS_ALLOW_NULL_yes, to_arg, opts->call);
    return x;
  }
  if (to == r_null) {
    // Allow `vec_cast(<vector>, NULL)`
    obj_check_vector(x, VCTRS_ALLOW_NULL_no, x_arg, opts->call);
    return x;
  }

  enum vctrs_type x_type = vec_typeof(x);
  enum vctrs_type to_type = vec_typeof(to);

  if (x_type == VCTRS_TYPE_unspecified) {
    return vec_init(to, vec_size(x));
  }

  if (x_type == VCTRS_TYPE_scalar) {
    stop_scalar_type(x, x_arg, opts->call);
  }
  if (to_type == VCTRS_TYPE_scalar) {
    stop_scalar_type(to, to_arg, opts->call);
  }

  r_obj* out = r_null;
  bool lossy = false;

  if (to_type == VCTRS_TYPE_s3 || x_type == VCTRS_TYPE_s3) {
    out = KEEP(vec_cast_dispatch_native(opts, x_type, to_type, &lossy));
  } else {
    out = KEEP(vec_cast_switch_native(opts, x_type, to_type, &lossy));
  }

  if (lossy || out == r_null) {
    // This broadcasts dimensions too
    FREE(1);
    return vec_cast_dispatch_s3(opts);
  }

  if (has_dim(x) || has_dim(to)) {
    out = vec_shape_broadcast(out, opts);
  }

  FREE(1);
  return out;
}

static
r_obj* vec_cast_switch_native(const struct cast_opts* opts,
                              enum vctrs_type x_type,
                              enum vctrs_type to_type,
                              bool* lossy) {
  r_obj* x = opts->x;

  int dir = 0;
  enum vctrs_type2 type2 = vec_typeof2_impl(x_type, to_type, &dir);

  switch (type2) {

  case VCTRS_TYPE2_logical_logical:
  case VCTRS_TYPE2_integer_integer:
  case VCTRS_TYPE2_double_double:
  case VCTRS_TYPE2_complex_complex:
  case VCTRS_TYPE2_raw_raw:
  case VCTRS_TYPE2_character_character:
  case VCTRS_TYPE2_list_list:
    return x;

  case VCTRS_TYPE2_logical_integer:
    if (dir == 0) {
      return lgl_as_integer(x, lossy);
    } else {
      return int_as_logical(x, lossy);
    }

  case VCTRS_TYPE2_logical_double:
    if (dir == 0) {
      return lgl_as_double(x, lossy);
    } else {
      return dbl_as_logical(x, lossy);
    }

  case VCTRS_TYPE2_integer_double:
    if (dir == 0) {
      return int_as_double(x, lossy);
    } else {
      return dbl_as_integer(x, lossy);
    }

  case VCTRS_TYPE2_dataframe_dataframe:
    return df_cast_opts(opts);

  default:
    break;
  }

  return r_null;
}


static inline
r_obj* vec_cast_default_full(r_obj* x,
                             r_obj* to,
                             struct vctrs_arg* p_x_arg,
                             struct vctrs_arg* p_to_arg,
                             struct r_lazy call,
                             enum s3_fallback s3_fallback,
                             bool from_dispatch) {
  r_obj* ffi_s3_fallback = KEEP(r_int(s3_fallback));

  r_obj* ffi_x_arg = KEEP(vctrs_arg(p_x_arg));
  r_obj* ffi_to_arg = KEEP(vctrs_arg(p_to_arg));

  r_obj* ffi_call = KEEP(r_lazy_eval(call));
  r_obj* out = vctrs_eval_mask7(syms.vec_default_cast,
                                syms_x, x,
                                syms_to, to,
                                syms_x_arg, ffi_x_arg,
                                syms_to_arg, ffi_to_arg,
                                syms_call, ffi_call,
                                syms_from_dispatch, r_lgl(from_dispatch),
                                syms_s3_fallback, ffi_s3_fallback);
  FREE(4);
  return out;
}

r_obj* vec_cast_default(r_obj* x,
                        r_obj* to,
                        struct vctrs_arg* p_x_arg,
                        struct vctrs_arg* p_to_arg,
                        struct r_lazy call,
                        enum s3_fallback s3_fallback) {
  return vec_cast_default_full(x, to, p_x_arg, p_to_arg, call, s3_fallback, false);
}

static
r_obj* vec_cast_dispatch_s3(const struct cast_opts* opts) {
  r_obj* x = opts->x;
  r_obj* to = opts->to;
  r_obj* method_sym = r_null;
  r_obj* method = s3_find_method_xy("vec_cast", to, x, vctrs_method_table, &method_sym);

  // Compatibility with legacy double dispatch mechanism
  if (method == r_null) {
    r_obj* to_method_sym = r_null;
    r_obj* to_method = KEEP(s3_find_method2("vec_cast",
                                             to,
                                             vctrs_method_table,
                                             &to_method_sym));

    if (to_method != r_null) {
      // Only `to_method`s contained within a package will
      // have an S3 methods table to look in
      r_obj* to_table = s3_get_table(r_fn_env(to_method));

      if (to_table != r_null) {
        const char* to_method_str = r_sym_c_string(to_method_sym);

        method = s3_find_method2(
          to_method_str,
          x,
          to_table,
          &method_sym
        );
      }
    }

    FREE(1);
  }

  KEEP(method);

  if (method == r_null) {
    r_obj* out = vec_cast_default_full(x,
                                       to,
                                       opts->p_x_arg,
                                       opts->p_to_arg,
                                       opts->call,
                                       opts->s3_fallback,
                                       true);
    FREE(1);
    return out;
  }

  r_obj* r_x_arg = KEEP(vctrs_arg(opts->p_x_arg));
  r_obj* r_to_arg = KEEP(vctrs_arg(opts->p_to_arg));

  r_obj* out = vec_invoke_coerce_method(method_sym, method,
                                        syms_x, x,
                                        syms_to, to,
                                        syms_x_arg, r_x_arg,
                                        syms_to_arg, r_to_arg,
                                        opts->call,
                                        opts->s3_fallback);

  FREE(3);
  return out;
}

struct cast_err_data {
  const struct cast_opts* opts;
  r_obj* out;
};

static
void vec_cast_e_cb(void* data_) {
  struct cast_err_data* data = (struct cast_err_data*) data_;
  data->out = vec_cast_opts(data->opts);
}

r_obj* vec_cast_e(const struct cast_opts* opts,
                  ERR* err) {
  struct cast_err_data data = {
    .opts = opts,
    .out = r_null
  };

  *err = r_try_catch(&vec_cast_e_cb,
                     &data,
                     syms_vctrs_error_incompatible_type,
                     NULL,
                     NULL);
  return data.out;
}

r_obj* vec_cast_common_opts(r_obj* xs,
                            r_obj* to,
                            const struct cast_common_opts* opts) {
  r_obj* type = KEEP(vec_ptype_common(
    xs,
    to,
    PTYPE_FINALISE_DEFAULT,
    opts->s3_fallback,
    opts->p_arg,
    opts->call
  ));

  const r_ssize xs_size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  r_obj* out = KEEP(r_alloc_list(xs_size));
  r_attrib_poke_names(out, r_names(xs));

  r_ssize i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    opts->p_arg,
    r_names(xs),
    xs_size,
    &i
  );
  KEEP(p_x_arg->shelter);

  for (; i < xs_size; ++i) {
    r_obj* elt = v_xs[i];
    struct cast_opts cast_opts = {
      .x = elt,
      .to = type,
      .p_x_arg = p_x_arg,
      .call = opts->call,
      .s3_fallback = opts->s3_fallback
    };
    r_list_poke(out, i, vec_cast_opts(&cast_opts));
  }

  FREE(3);
  return out;
}
r_obj* vec_cast_common_params(r_obj* xs,
                              r_obj* to,
                              enum s3_fallback s3_fallback,
                              struct vctrs_arg* p_arg,
                              struct r_lazy call) {
  struct cast_common_opts opts = {
    .p_arg = p_arg,
    .call = call,
    .s3_fallback = s3_fallback
  };
  return vec_cast_common_opts(xs, to, &opts);
}

r_obj* vec_cast_common(r_obj* xs,
                       r_obj* to,
                       struct vctrs_arg* p_arg,
                       struct r_lazy call) {
  return vec_cast_common_params(xs,
                                to,
                                S3_FALLBACK_DEFAULT,
                                p_arg,
                                call);
}

// [[ register(external = TRUE) ]]
r_obj* ffi_cast_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* to = r_node_car(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };

  struct r_lazy xs_arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  r_obj* out = vec_cast_common(xs, to, &xs_arg, call);

  return out;
}

// [[ register(external = TRUE) ]]
r_obj* ffi_cast_common_opts(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* to = r_node_car(args); args = r_node_cdr(args);
  r_obj* ffi_opts = r_node_car(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };

  struct r_lazy xs_arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  struct cast_common_opts opts = {
    .p_arg = &xs_arg,
    .call = call,
    .s3_fallback = s3_fallback_from_opts(ffi_opts)
  };

  r_obj* out = vec_cast_common_opts(xs, to, &opts);

  return out;
}

struct cast_opts new_cast_opts(r_obj* x,
                               r_obj* to,
                               struct vctrs_arg* p_x_arg,
                               struct vctrs_arg* p_to_arg,
                               struct r_lazy call,
                               r_obj* opts) {
  return (struct cast_opts) {
    .x = x,
    .to = to,
    .p_x_arg = p_x_arg,
    .p_to_arg = p_to_arg,
    .call = call,
    .s3_fallback = s3_fallback_from_opts(opts)
  };
}


void vctrs_init_cast(r_obj* ns) {
  syms.vec_default_cast = r_sym("vec_default_cast");
}
