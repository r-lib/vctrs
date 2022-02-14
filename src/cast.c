#include "vctrs.h"
#include "dim.h"
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
  struct vctrs_arg* x_arg = opts->x_arg;
  struct vctrs_arg* to_arg = opts->to_arg;

  if (x == r_null) {
    if (!vec_is_partial(to)) {
      vec_check_vector(to, to_arg, opts->call);
    }
    return x;
  }
  if (to == r_null) {
    if (!vec_is_partial(x)) {
      vec_check_vector(x, x_arg, opts->call);
    }
    return x;
  }

  enum vctrs_type x_type = vec_typeof(x);
  enum vctrs_type to_type = vec_typeof(to);

  if (x_type == vctrs_type_unspecified) {
    return vec_init(to, vec_size(x));
  }

  if (x_type == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg, opts->call);
  }
  if (to_type == vctrs_type_scalar) {
    stop_scalar_type(to, to_arg, opts->call);
  }

  if (has_dim(x) || has_dim(to)) {
    return vec_cast_dispatch_s3(opts);
  }

  r_obj* out = r_null;
  bool lossy = false;

  if (to_type == vctrs_type_s3 || x_type == vctrs_type_s3) {
    out = vec_cast_dispatch_native(opts, x_type, to_type, &lossy);
  } else {
    out = vec_cast_switch_native(opts, x_type, to_type, &lossy);
  }

  if (lossy || out == r_null) {
    return vec_cast_dispatch_s3(opts);
  } else {
    return out;
  }
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

  case vctrs_type2_logical_logical:
  case vctrs_type2_integer_integer:
  case vctrs_type2_double_double:
  case vctrs_type2_complex_complex:
  case vctrs_type2_raw_raw:
  case vctrs_type2_character_character:
  case vctrs_type2_list_list:
    return x;

  case vctrs_type2_logical_integer:
    if (dir == 0) {
      return lgl_as_integer(x, lossy);
    } else {
      return int_as_logical(x, lossy);
    }

  case vctrs_type2_logical_double:
    if (dir == 0) {
      return lgl_as_double(x, lossy);
    } else {
      return dbl_as_logical(x, lossy);
    }

  case vctrs_type2_integer_double:
    if (dir == 0) {
      return int_as_double(x, lossy);
    } else {
      return dbl_as_integer(x, lossy);
    }

  case vctrs_type2_dataframe_dataframe:
    return df_cast_opts(opts);

  default:
    break;
  }

  return r_null;
}


r_obj* vec_cast_default(r_obj* x,
                        r_obj* to,
                        r_obj* x_arg,
                        r_obj* to_arg,
                        struct r_lazy call,
                        const struct fallback_opts* opts) {
  r_obj* df_fallback = KEEP(r_int(opts->df));
  r_obj* s3_fallback = KEEP(r_int(opts->s3));
  r_obj* ffi_call = KEEP(r_lazy_eval(call));
  r_obj* out = vctrs_eval_mask8(syms.vec_default_cast,
                                syms_x, x,
                                syms_to, to,
                                syms_x_arg, x_arg,
                                syms_to_arg, to_arg,
                                syms_call, ffi_call,
                                syms_from_dispatch, vctrs_shared_true,
                                syms_df_fallback, df_fallback,
                                syms_s3_fallback, s3_fallback);
  FREE(3);
  return out;
}

static
r_obj* vec_cast_dispatch_s3(const struct cast_opts* opts) {
  r_obj* x = opts->x;
  r_obj* to = opts->to;
  r_obj* r_x_arg = KEEP(vctrs_arg(opts->x_arg));
  r_obj* r_to_arg = KEEP(vctrs_arg(opts->to_arg));

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
      const char* to_method_str = CHAR(PRINTNAME(to_method_sym));
      r_obj* to_table = s3_get_table(CLOENV(to_method));

      method = s3_find_method2(to_method_str,
                               x,
                               to_table,
                               &method_sym);
    }

    FREE(1);
  }

  KEEP(method);

  if (method == r_null) {
    r_obj* out = vec_cast_default(x,
                                to,
                                r_x_arg,
                                r_to_arg,
                                opts->call,
                                &(opts->fallback));
    FREE(3);
    return out;
  }

  r_obj* out = vec_invoke_coerce_method(method_sym, method,
                                        syms_x, x,
                                        syms_to, to,
                                        syms_x_arg, r_x_arg,
                                        syms_to_arg, r_to_arg,
                                        opts->call,
                                        &(opts->fallback));

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
                     syms_vctrs_error_cast_lossy,
                     NULL,
                     NULL);
  return data.out;
}

r_obj* vec_cast_common_opts(r_obj* xs,
                            r_obj* to,
                            const struct cast_common_opts* opts) {
  struct ptype_common_opts ptype_opts = {
    .call = opts->call,
    .fallback = opts->fallback
  };
  r_obj* type = KEEP(vec_ptype_common_opts(xs, to, &ptype_opts));

  r_ssize n = r_length(xs);
  r_obj* out = KEEP(r_alloc_list(n));

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = r_list_get(xs, i);
    struct cast_opts cast_opts = {
      .x = elt,
      .to = type,
      .call = opts->call,
      .fallback = opts->fallback
    };
    r_list_poke(out, i, vec_cast_opts(&cast_opts));
  }

  r_attrib_poke_names(out, r_names(xs));

  FREE(2);
  return out;
}
r_obj* vec_cast_common_params(r_obj* xs,
                              r_obj* to,
                              enum df_fallback df_fallback,
                              enum s3_fallback s3_fallback,
                              struct r_lazy call) {
  struct cast_common_opts opts = {
    .call = call,
    .fallback = {
      .df = df_fallback,
      .s3 = s3_fallback
    }
  };
  return vec_cast_common_opts(xs, to, &opts);
}

r_obj* vec_cast_common(r_obj* xs, r_obj* to, struct r_lazy call) {
  return vec_cast_common_params(xs,
                                to,
                                DF_FALLBACK_DEFAULT,
                                S3_FALLBACK_DEFAULT,
                                call);
}

// [[ register(external = TRUE) ]]
r_obj* ffi_cast_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* dots = KEEP(rlang_env_dots_list(env));
  r_obj* to = KEEP(r_eval(r_node_car(args), env));
  struct r_lazy call = { .x = syms.dot_call, .env = env };

  r_obj* out = vec_cast_common(dots, to, call);

  FREE(2);
  return out;
}

// [[ register(external = TRUE) ]]
r_obj* ffi_cast_common_opts(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* dots = KEEP(rlang_env_dots_list(env));
  r_obj* to = KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  r_obj* ffi_fallback_opts = KEEP(r_eval(r_node_car(args), env));

  struct cast_common_opts opts = {
    .call = { .x = syms.dot_call, .env = env },
    .fallback = new_fallback_opts(ffi_fallback_opts)
  };

  r_obj* out = vec_cast_common_opts(dots, to, &opts);

  FREE(3);
  return out;
}

struct cast_opts new_cast_opts(r_obj* x,
                               r_obj* to,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* to_arg,
                               struct r_lazy call,
                               r_obj* opts) {
  return (struct cast_opts) {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg,
    .call = call,
    .fallback = {
      .df = r_int_get(r_list_get(opts, 0), 0),
      .s3 = r_int_get(r_list_get(opts, 1), 0)
    }
  };
}


void vctrs_init_cast(r_obj* ns) {
  syms.vec_default_cast = r_sym("vec_default_cast");
}
