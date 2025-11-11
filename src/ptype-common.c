#include "vctrs.h"

struct ptype_common_reduce_opts {
  struct r_lazy call;
  enum s3_fallback s3_fallback;
};

#include "decl/ptype-common-decl.h"

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* ptype = r_node_car(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy xs_arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  r_obj* out = vec_ptype_common(
    xs,
    ptype,
    S3_FALLBACK_false,
    &xs_arg,
    call
  );

  return out;
}

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common_opts(r_obj* call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* ptype = r_node_car(args); args = r_node_cdr(args);
  r_obj* opts = r_node_car(args);

  struct ptype_common_opts ptype_opts = {
    .call = { .x = syms.dot_call, .env = env },
    .s3_fallback = s3_fallback_from_opts(opts)
  };
  r_obj* out = vec_ptype_common_opts(xs, ptype, &ptype_opts);

  return out;
}

r_obj* vec_ptype_common_opts(r_obj* dots,
                             r_obj* ptype,
                             const struct ptype_common_opts* opts) {
  return vec_ptype_common(
    dots,
    ptype,
    opts->s3_fallback,
    opts->p_arg,
    opts->call
  );
}

r_obj* vec_ptype_common(
  r_obj* dots,
  r_obj* ptype,
  enum s3_fallback s3_fallback,
  struct vctrs_arg* p_arg,
  struct r_lazy call
) {
  if (!vec_is_partial(ptype)) {
    return vec_ptype(ptype, vec_args.dot_ptype, call);
  }

  if (r_is_true(r_peek_option("vctrs.no_guessing"))) {
    r_abort_lazy_call(r_lazy_null, "strict mode is activated; you must supply complete `.ptype`.");
  }

  struct ptype_common_reduce_opts reduce_opts = {
    .call = call,
    .s3_fallback = s3_fallback
  };

  // Start reduction with the `.ptype` argument
  r_obj* type = KEEP(reduce(
    ptype,
    vec_args.dot_ptype,
    p_arg,
    dots,
    &ptype2_common,
    &reduce_opts
  ));
  type = vec_ptype_finalise(type);

  FREE(1);
  return type;
}

static
r_obj* ptype2_common(r_obj* current,
                     r_obj* next,
                     struct counters* counters,
                     void* p_data) {
  int left = -1;

  struct ptype_common_reduce_opts* p_reduce_opts = (struct ptype_common_reduce_opts*) p_data;

  const struct ptype2_opts opts = {
    .x = current,
    .y = next,
    .p_x_arg = counters->curr_arg,
    .p_y_arg = counters->next_arg,
    .call = p_reduce_opts->call,
    .s3_fallback = p_reduce_opts->s3_fallback
  };

  current = vec_ptype2_opts(&opts, &left);

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_shift(counters);
  }

  return current;
}
