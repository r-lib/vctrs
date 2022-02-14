#include "vctrs.h"
#include "ptype2.h"
#include "decl/ptype-common-decl.h"

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common(r_obj* call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* types = KEEP(rlang_env_dots_values(env));
  r_obj* ptype = KEEP(r_eval(r_node_car(args), env));

  r_obj* out = vec_ptype_common_params(types, ptype, DF_FALLBACK_DEFAULT, S3_FALLBACK_false);

  FREE(2);
  return out;
}

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common_opts(r_obj* call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* types = KEEP(rlang_env_dots_values(env));
  r_obj* ptype = KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  r_obj* opts = KEEP(r_eval(r_node_car(args), env));

  const struct fallback_opts c_opts = new_fallback_opts(opts);
  r_obj* out = vec_ptype_common_opts(types, ptype, &c_opts);

  FREE(3);
  return out;
}

r_obj* vec_ptype_common_opts(r_obj* dots,
                             r_obj* ptype,
                             const struct fallback_opts* opts) {
  // FIXME! Error call
  if (!vec_is_partial(ptype)) {
    return vec_ptype(ptype, args_dot_ptype, r_lazy_null);
  }

  if (r_is_true(r_peek_option("vctrs.no_guessing"))) {
    r_abort_lazy_call(r_lazy_null, "strict mode is activated; you must supply complete `.ptype`.");
  }

  // Remove constness
  struct fallback_opts mut_opts = *opts;

  // Start reduction with the `.ptype` argument
  r_obj* type = KEEP(reduce(ptype, args_dot_ptype, dots, &ptype2_common, &mut_opts));
  type = vec_ptype_finalise(type);

  FREE(1);
  return type;
}

r_obj* vec_ptype_common_params(r_obj* dots,
                               r_obj* ptype,
                               enum df_fallback df_fallback,
                               enum s3_fallback s3_fallback) {
  struct fallback_opts opts = {
    .df = df_fallback,
    .s3 = s3_fallback
  };

  return vec_ptype_common_opts(dots, ptype, &opts);
}

static
r_obj* ptype2_common(r_obj* current,
                     r_obj* next,
                     struct counters* counters,
                     void* data) {
  int left = -1;

  const struct ptype2_opts opts = {
    .x = current,
    .y = next,
    .x_arg = counters->curr_arg,
    .y_arg = counters->next_arg,
    .fallback = *((struct fallback_opts*) data)
  };

  current = vec_ptype2_opts(&opts, &left);

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_shift(counters);
  }

  return current;
}
