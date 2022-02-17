#include "vctrs.h"
#include "decl/ptype-common-decl.h"

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* types = KEEP(rlang_env_dots_values(env));
  r_obj* ptype = KEEP(r_eval(r_node_car(args), env));

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  r_obj* out = vec_ptype_common_params(types,
                                       ptype,
                                       DF_FALLBACK_DEFAULT,
                                       S3_FALLBACK_false,
                                       &arg,
                                       call);

  FREE(2);
  return out;
}

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common_opts(r_obj* call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* types = KEEP(rlang_env_dots_values(env));
  r_obj* ptype = KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  r_obj* opts = KEEP(r_eval(r_node_car(args), env));

  struct ptype_common_opts ptype_opts = {
    .call = { .x = syms.dot_call, .env = env },
    .fallback = new_fallback_opts(opts)
  };
  r_obj* out = vec_ptype_common_opts(types, ptype, &ptype_opts);

  FREE(3);
  return out;
}

r_obj* vec_ptype_common_opts(r_obj* dots,
                             r_obj* ptype,
                             const struct ptype_common_opts* opts) {
  if (!vec_is_partial(ptype)) {
    return vec_ptype(ptype, args_dot_ptype, opts->call);
  }

  if (r_is_true(r_peek_option("vctrs.no_guessing"))) {
    r_abort_lazy_call(r_lazy_null, "strict mode is activated; you must supply complete `.ptype`.");
  }

  // Remove constness
  struct ptype_common_opts mut_opts = *opts;

  // Start reduction with the `.ptype` argument
  r_obj* type = KEEP(reduce(ptype,
                            args_dot_ptype,
                            mut_opts.p_arg,
                            dots,
                            &ptype2_common,
                            &mut_opts));
  type = vec_ptype_finalise(type);

  FREE(1);
  return type;
}

r_obj* vec_ptype_common_params(r_obj* dots,
                               r_obj* ptype,
                               enum df_fallback df_fallback,
                               enum s3_fallback s3_fallback,
                               struct vctrs_arg* p_arg,
                               struct r_lazy call) {
  struct ptype_common_opts opts = {
    .call = call,
    .p_arg = p_arg,
    .fallback = {
      .df = df_fallback,
      .s3 = s3_fallback
    }
  };

  return vec_ptype_common_opts(dots, ptype, &opts);
}

static
r_obj* ptype2_common(r_obj* current,
                     r_obj* next,
                     struct counters* counters,
                     void* p_data) {
  int left = -1;

  struct ptype_common_opts* p_common_opts = (struct ptype_common_opts*) p_data;

  const struct ptype2_opts opts = {
    .x = current,
    .y = next,
    .p_x_arg = counters->curr_arg,
    .p_y_arg = counters->next_arg,
    .call = p_common_opts->call,
    .fallback = p_common_opts->fallback
  };

  current = vec_ptype2_opts(&opts, &left);

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_shift(counters);
  }

  return current;
}
