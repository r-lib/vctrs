#include "vctrs.h"
#include "decl/size-common-decl.h"

struct size_common_reduce_opts {
  // Updated at each iteration.
  // Allows us to reuse `vec_size()` info from the previous iteration.
  r_ssize current_size;
  const struct r_lazy call;
};

// [[ register(external = TRUE) ]]
r_obj* ffi_size_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  struct r_lazy internal_call = { .x = env, .env = r_null };

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* size = r_node_car(args); args = r_node_cdr(args);
  r_obj* absent = r_node_car(args);

  if (size != r_null) {
    r_ssize out = vec_as_short_length(size,
                                      vec_args.dot_size,
                                      internal_call);
    return r_int(out);
  }

  if (absent != r_null && (r_typeof(absent) != R_TYPE_integer || r_length(absent) != 1)) {
    r_abort_lazy_call(internal_call,
                      "%s must be a single integer.",
                      r_c_str_format_error_arg(".absent"));
  }

  r_ssize common = vec_size_common(xs, -1, &arg, call);

  r_obj* out;
  if (common < 0) {
    if (absent == r_null) {
      r_abort_lazy_call(internal_call,
                        "%s must be supplied when %s is empty.",
                        r_c_str_format_error_arg(".absent"),
                        r_c_str_format_error_arg("..."));
    }
    out = absent;
  } else {
    out = r_int(common);
  }

  return out;
}

r_ssize vec_size_common(
  r_obj* xs,
  r_ssize absent,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
) {
  struct size_common_reduce_opts reduce_opts = {
    .current_size = -1,
    .call = call
  };

  // Interested in `reduce_opts.current_size`,
  // not in the returned `r_obj*` from `reduce()`
  reduce(
    r_null,
    vec_args.empty,
    p_xs_arg,
    xs,
    &size2_common,
    &reduce_opts
  );

  r_ssize out = reduce_opts.current_size;

  if (out == -1) {
    out = absent;
  }

  return out;
}

/**
 * `vec_size2()` implementation
 *
 * `left` works the same as `vec_typeof2_impl()`
 *
 * @param left Output parameter. Set to 1 when the common size comes
 *   from the left, 0 when it comes from the right, and -1 when it
 *   comes from both sides. This means that "left" is the default
 *   when coerced to a boolean value.
*/
static inline
r_obj* vec_size2_impl(
  r_obj* x,
  r_obj* y,
  r_ssize x_size,
  r_ssize y_size,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  int* left
) {
  // `NULL` handling rules:
  // - If `x` and `y` are `NULL`, do nothing
  // - If `x` is `NULL`, use `y`
  // - If `y` is `NULL`, use `x`
  //
  // The first rule is important to ensure that this works
  // `vec_size_common(NULL, .absent = 5L)`
  if (x == r_null) {
    if (y == r_null) {
      *left = -1;
      return x;
    } else {
      *left = 0;
      return y;
    }
  }
  if (y == r_null) {
    if (x == r_null) {
      r_stop_unreachable();
    } else {
      *left = 1;
      return x;
    }
  }

  // Now apply common size rules
  // - Same size, use `x`
  // - Size 1 `x`, use `y`
  // - Size 1 `y`, use `x`
  if (x_size == y_size) {
    *left = -1;
    return x;
  }
  if (x_size == 1) {
    *left = 0;
    return y;
  }
  if (y_size == 1) {
    *left = 1;
    return x;
  }

  stop_incompatible_size(
    x,
    y,
    x_size,
    y_size,
    p_x_arg,
    p_y_arg,
    call
  );
}

// Size2 computation
//
// `reduce_opts->current_size` updates when we switch to `y`
static
r_obj* size2_common(
  r_obj* x,
  r_obj* y,
  struct counters* counters,
  void* data
) {
  struct size_common_reduce_opts* reduce_opts = data;

  const r_ssize x_size = reduce_opts->current_size;
  const r_ssize y_size = vec_size_3(y, counters->next_arg, reduce_opts->call);

  int left = -1;

  r_obj* out = vec_size2_impl(
    x,
    y,
    x_size,
    y_size,
    counters->curr_arg,
    counters->next_arg,
    reduce_opts->call,
    &left
  );

  if (!left) {
    counters_shift(counters);
    reduce_opts->current_size = y_size;
  }

  return out;
}

// [[ register(external = TRUE) ]]
r_obj* ffi_recycle_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy internal_call = { .x = env, .env = r_null };

  struct r_lazy arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  r_obj* size = r_node_car(args); args = r_node_cdr(args);
  r_obj* xs = KEEP(rlang_env_dots_list(env));

  r_ssize common;
  if (size == r_null) {
    common = vec_size_common(xs, -1, &arg, call);
  } else {
    common = vec_as_short_length(size,
                                 vec_args.dot_size,
                                 internal_call);
  }

  r_obj* out = vec_recycle_common(xs, common, &arg, call);

  FREE(1);
  return out;
}

r_obj* vec_recycle_common(
  r_obj* xs,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
) {
  if (size < 0) {
    return xs;
  }

  xs = KEEP(r_clone_referenced(xs));
  const r_ssize n = vec_size(xs);

  r_ssize i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    p_xs_arg,
    r_names(xs),
    n,
    &i
  );
  KEEP(p_x_arg->shelter);

  for (; i < n; ++i) {
    r_obj* elt = r_list_get(xs, i);

    elt = vec_recycle(
      elt,
      size,
      p_x_arg,
      call
    );

    r_list_poke(xs, i, elt);
  }

  FREE(2);
  return xs;
}
