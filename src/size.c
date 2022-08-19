#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/size-decl.h"

// [[ register() ]]
r_obj* ffi_size(r_obj* x, r_obj* frame) {
  struct vec_error_opts err = {
    .p_arg = vec_args.x,
    .call = { .x = frame, .env = r_null }
  };
  return r_len(vec_size_opts(x, &err));
}

r_ssize vec_size(r_obj* x) {
  struct vec_error_opts err = {
    .p_arg = vec_args.x,
    .call = lazy_calls.vec_size
  };
  return vec_size_opts(x, &err);
}

static
r_ssize vec_size_opts(r_obj* x, const struct vec_error_opts* opts) {
  struct vctrs_proxy_info info = vec_proxy_info(x);
  KEEP(info.shelter);

  r_obj* data = info.proxy;

  r_ssize size;
  switch (info.type) {
  case vctrs_type_null:
    size = 0;
    break;
  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list:
    size = vec_raw_size(data);
    break;

  case vctrs_type_dataframe:
    size = df_size(data);
    break;

  default:
    stop_scalar_type(x, opts->p_arg, opts->call);
}

  FREE(1);
  return size;
}

static
r_ssize vec_raw_size(r_obj* x) {
  r_obj* dimensions = r_dim(x);

  if (dimensions == r_null || r_length(dimensions) == 0) {
    return r_length(x);
  }

  if (r_typeof(dimensions) != R_TYPE_integer) {
    r_stop_internal("Corrupt vector, `dim` attribute is not an integer vector.");
  }

  return r_int_get(dimensions, 0);
}

// [[ register() ]]
r_obj* ffi_list_sizes(r_obj* x, r_obj* frame) {
  struct vec_error_opts err = {
    .p_arg = vec_args.x,
    .call = { .x = frame, .env = r_null }
  };
  return list_sizes(x, &err);
}

static
r_obj* list_sizes(r_obj* x, const struct vec_error_opts* opts) {
  if (!vec_is_list(x)) {
    r_abort_lazy_call(opts->call,
                      "%s must be a list, not %s.",
                      r_c_str_format_error_arg("x"),
                      r_friendly_type_of(x));
  }

  r_ssize size = vec_size(x);
  r_obj* const * v_x = r_list_cbegin(x);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  r_obj* names = vec_names(x);
  r_attrib_poke_names(out, names);

  r_ssize i = 0;
  struct vctrs_arg* arg = new_subscript_arg_vec(opts->p_arg, x, &i);
  KEEP(arg->shelter);

  struct vec_error_opts local_opts = *opts;
  local_opts.p_arg = arg;

  for (; i < size; ++i) {
    v_out[i] = vec_size_opts(v_x[i], &local_opts);
  }

  FREE(2);
  return out;
}

r_ssize df_rownames_size(r_obj* x) {
  for (r_obj* attr = r_attrib(x);
       attr != r_null;
       attr = r_node_cdr(attr)) {
    if (r_node_tag(attr) != r_syms.row_names) {
      continue;
    }

    return rownames_size(r_node_car(attr));
  }

  return -1;
}

// For performance, avoid Rf_getAttrib() because it automatically transforms
// the rownames into an integer vector
r_ssize df_size(r_obj* x) {
  r_ssize n = df_rownames_size(x);

  if (n < 0) {
    r_stop_internal("Corrupt data frame: row.names are missing");
  }

  return n;
}
// Supports bare lists as well
r_ssize df_raw_size(r_obj* x) {
  r_ssize n = df_rownames_size(x);
  if (n >= 0) {
    return n;
  }

  return df_raw_size_from_list(x);
}

r_ssize df_raw_size_from_list(r_obj* x) {
  if (r_length(x) >= 1) {
    return vec_size(r_list_get(x, 0));
  } else {
    return 0;
  }
}

// [[ register() ]]
SEXP vctrs_df_size(SEXP x) {
  return r_int(df_raw_size(x));
}

r_obj* vec_check_recycle(r_obj* x,
                         r_ssize size,
                         struct vctrs_arg* x_arg,
                         struct r_lazy call) {
  if (x == r_null) {
    return r_null;
  }

  r_ssize n_x = vec_size(x);

  if (n_x == size) {
    return x;
  }

  if (n_x == 1L) {
    r_obj* i = KEEP(compact_rep(1, size));
    r_obj* out = vec_slice_unsafe(x, i);

    FREE(1);
    return out;
  }

  stop_recycle_incompatible_size(n_x, size, x_arg, call);
}

// [[ register() ]]
r_obj* ffi_recycle(r_obj* x,
                   r_obj* size_obj,
                   r_obj* frame) {
  if (x == r_null || size_obj == r_null) {
    return r_null;
  }

  struct r_lazy recycle_call = { .x = frame, .env = r_null };

  size_obj = KEEP(vec_cast(size_obj,
                           vctrs_shared_empty_int,
                           vec_args.empty,
                           vec_args.empty,
                           recycle_call));
  R_len_t size = r_int_get(size_obj, 0);
  FREE(1);

  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy call = { .x = syms_call, .env = frame };

  return vec_check_recycle(x, size, &x_arg, call);
}

r_obj* vec_recycle_fallback(r_obj* x,
                            r_ssize size,
                            struct vctrs_arg* x_arg) {
  if (x == r_null) {
    return r_null;
  }

  r_ssize x_size = vec_size(x);

  if (x_size == size) {
    return x;
  }

  if (x_size == 1) {
    r_obj* subscript = KEEP(r_alloc_integer(size));
    r_int_fill(subscript, 1, size);

    r_obj* out = vec_slice_fallback(x, subscript);

    FREE(1);
    return out;
  }

  stop_recycle_incompatible_size(x_size, size, x_arg, r_lazy_null);
}

r_obj* ffi_as_short_length(r_obj* n, r_obj* frame) {
  struct r_lazy call = { .x = frame, .env = r_null };

  struct r_lazy arg_lazy = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  return r_len(vec_as_short_length(n, &arg, call));
}

r_ssize vec_as_short_length(r_obj* n,
                            struct vctrs_arg* p_arg,
                            struct r_lazy call) {
  r_ssize out = vec_as_ssize(n, p_arg, call);

  if (out < 0) {
    r_abort_lazy_call(call,
                      "%s must be a positive number or zero.",
                      vec_arg_format(p_arg));
  }

  if (out > INT_MAX) {
    // Ideally we'd mention long vector support in an info bullets
    r_abort_lazy_call(call,
                      "%s is too large a number and long vectors are not supported.",
                      vec_arg_format(p_arg));
  }

  return out;
}

// Adapted from `r_arg_as_ssize()`
r_ssize vec_as_ssize(r_obj* n,
                     struct vctrs_arg* p_arg,
                     struct r_lazy call) {
  if (r_is_object(n)) {
    struct cast_opts cast_opts = {
      .x = n,
      .to = r_globals.empty_dbl,
      .p_x_arg = p_arg,
      .call = call
    };
    ERR err = NULL;
    n = vec_cast_e(&cast_opts, &err);

    if (err) {
      goto invalid;
    }
  }
  KEEP(n);

  switch (r_typeof(n)) {

  case R_TYPE_double: {
    if (r_length(n) != 1) {
      goto invalid;
    }
    double out = r_dbl_get(n, 0);

    if (out == r_globals.na_int) {
      goto invalid;
    }
    if (out != floor(out)) {
      r_abort_lazy_call(call,
                        "%s must be a whole number, not a fractional number.",
                        vec_arg_format(p_arg));
    }

    if (out > R_SSIZE_MAX) {
      r_abort_lazy_call(call,
                        "%s is too large a number.",
                        vec_arg_format(p_arg));
    }

    FREE(1);
    return (r_ssize) out;
  }

  case R_TYPE_integer: {
    if (r_length(n) != 1) {
      goto invalid;
    }
    int out = r_int_get(n, 0);

    if (out == r_globals.na_int) {
      goto invalid;
    }

    FREE(1);
    return (r_ssize) out;
  }

  invalid:
  default:
    r_abort_lazy_call(call,
                      "%s must be a single number, not %s.",
                      vec_arg_format(p_arg),
                      r_friendly_type_of_length(n));
  }
}
