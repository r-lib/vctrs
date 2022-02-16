#include "vctrs.h"
#include "decl/size-common-decl.h"


// [[ register(external = TRUE) ]]
r_obj* ffi_size_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);
  struct r_lazy call = { .x = env, .env = r_null };

  // TODO! arg

  r_obj* size = r_node_car(args); args = r_node_cdr(args);
  r_obj* absent = r_node_car(args);

  if (size != r_null) {
    r_ssize out = check_size(size, vec_args.dot_size, call);
    return r_int(out);
  }

  if (absent != r_null && (r_typeof(absent) != R_TYPE_integer || r_length(absent) != 1)) {
    r_abort_lazy_call(call,
                      "%s must be a single integer.",
                      r_c_str_format_error_arg(".absent"));
  }

  r_obj* xs = KEEP(rlang_env_dots_list(env));
  r_ssize common = vec_size_common(xs, -1);

  r_obj* out;
  if (common < 0) {
    if (absent == r_null) {
      r_abort_lazy_call(call,
                        "%s is empty, and no %s value was supplied.",
                        r_c_str_format_error_arg("..."),
                        r_c_str_format_error_arg(".absent"));
    }
    out = absent;
  } else {
    out = r_int(common);
  }

  FREE(1);
  return out;
}

r_ssize vec_size_common(r_obj* xs, r_ssize absent) {
  r_obj* common = KEEP(reduce(r_null, args_empty, NULL, xs, &vctrs_size2_common, NULL));
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
                         counters->next_arg);
}

// [[ register(external = TRUE) ]]
r_obj* ffi_recycle_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  // TODO! arg
  // TODO! call
  struct r_lazy call = r_lazy_null;

  r_obj* size = r_node_car(args); args = r_node_cdr(args);
  r_obj* xs = KEEP(rlang_env_dots_list(env));

  r_ssize common;
  if (size != r_null) {
    common = check_size(size, vec_args.dot_size, call);
  } else {
    common = vec_size_common(xs, -1);
  }

  r_obj* out = KEEP(vec_recycle_common(xs, common));

  FREE(2);
  return out;
}

r_obj* vec_recycle_common(r_obj* xs, r_ssize size) {
  if (size < 0) {
    return xs;
  }

  xs = KEEP(r_clone_referenced(xs));
  r_ssize n = vec_size(xs);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = r_list_get(xs, i);
    r_list_poke(xs, i, vec_recycle(elt, size, args_empty));
  }

  FREE(1);
  return xs;
}
