#include "vctrs.h"
#include "decl/size-common-decl.h"


// [[ register(external = TRUE) ]]
r_obj* ffi_size_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  r_obj* size = r_node_car(args); args = r_node_cdr(args);
  r_obj* absent = r_node_car(args);

  if (size != r_null) {
    r_ssize out = vec_as_short_length(size, vec_args.dot_size, call);
    return r_int(out);
  }

  if (absent != r_null && (r_typeof(absent) != R_TYPE_integer || r_length(absent) != 1)) {
    r_abort_lazy_call(call,
                      "%s must be a single integer.",
                      r_c_str_format_error_arg(".absent"));
  }

  struct size_common_opts size_opts = {
    .p_arg = &arg,
    .call = call
  };

  r_obj* xs = KEEP(rlang_env_dots_list(env));
  r_ssize common = vec_size_common_opts(xs, -1, &size_opts);

  r_obj* out;
  if (common < 0) {
    if (absent == r_null) {
      r_abort_lazy_call(call,
                        "%s must be supplied when %s is empty.",
                        r_c_str_format_error_arg(".absent"),
                        r_c_str_format_error_arg("..."));
    }
    out = absent;
  } else {
    out = r_int(common);
  }

  FREE(1);
  return out;
}

r_ssize vec_size_common(r_obj* xs, r_ssize absent) {
  struct size_common_opts opts = { 0 };
  return vec_size_common_opts(xs, absent, &opts);
}

r_ssize vec_size_common_opts(r_obj* xs,
                             r_ssize absent,
                             const struct size_common_opts* opts) {
  struct size_common_opts mut_opts = *opts;

  r_obj* common = KEEP(reduce(r_null,
                              args_empty,
                              opts->p_arg,
                              xs,
                              &vctrs_size2_common,
                              &mut_opts));
  r_ssize out;

  if (common == r_null) {
    out = absent;
  } else {
    out = vec_size(common);
  }

  FREE(1);
  return out;
}

static
r_obj* vctrs_size2_common(r_obj* x,
                          r_obj* y,
                          struct counters* counters,
                          void* data) {
  struct size_common_opts* opts = data;

  if (x == r_null) {
    counters_shift(counters);
    return y;
  }
  if (y == r_null) {
    return x;
  }

  r_ssize nx = vec_size(x);
  r_ssize ny = vec_size(y);

  if (nx == ny) {
    return x;
  }
  if (nx == 1) {
    counters_shift(counters);
    return y;
  }
  if (ny == 1) {
    return x;
  }

  stop_incompatible_size(x,
                         y,
                         nx,
                         ny,
                         counters->curr_arg,
                         counters->next_arg,
                         opts->call);
}

// [[ register(external = TRUE) ]]
r_obj* ffi_recycle_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  struct size_common_opts size_opts = {
    .p_arg = &arg,
    .call = call
  };

  r_obj* size = r_node_car(args); args = r_node_cdr(args);
  r_obj* xs = KEEP(rlang_env_dots_list(env));

  r_ssize common;
  if (size == r_null) {
    common = vec_size_common_opts(xs, -1, &size_opts);
  } else {
    common = vec_as_short_length(size, vec_args.dot_size, call);
  }

  r_obj* out = vec_recycle_common_opts(xs, common, &size_opts);

  FREE(1);
  return out;
}

r_obj* vec_recycle_common(r_obj* xs, r_ssize size) {
  struct size_common_opts opts = { 0 };
  return vec_recycle_common_opts(xs, size, &opts);
}

r_obj* vec_recycle_common_opts(r_obj* xs,
                               r_ssize size,
                               const struct size_common_opts* p_opts) {
  if (size < 0) {
    return xs;
  }

  xs = KEEP(r_clone_referenced(xs));
  r_ssize n = vec_size(xs);

  r_ssize i = 0;
  struct vctrs_arg* p_x_arg = new_subscript_arg(p_opts->p_arg,
                                                r_names(xs),
                                                n,
                                                &i);
  KEEP(p_x_arg->shelter);

  for (; i < n; ++i) {
    r_obj* elt = r_list_get(xs, i);
    r_list_poke(xs, i, vec_recycle(elt, size, p_x_arg));
  }

  FREE(2);
  return xs;
}
