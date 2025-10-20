#include "vctrs.h"

r_obj* ffi_vec_c(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* frame) {
  args = r_node_cdr(args);

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* ptype = r_node_car(args); args = r_node_cdr(args);
  r_obj* name_spec = r_node_car(args); args = r_node_cdr(args);
  r_obj* name_repair = r_node_car(args);

  struct r_lazy error_arg_lazy = { .x = syms.dot_error_arg, .env = frame };
  struct vctrs_arg error_arg = new_lazy_arg(&error_arg_lazy);

  struct r_lazy error_call = { .x = syms.dot_error_call, .env = frame };

  struct name_repair_opts name_repair_opts = new_name_repair_opts(
    name_repair,
    r_lazy_null,
    false,
    error_call
  );
  KEEP(name_repair_opts.shelter);

  r_obj* out = vec_c(xs, ptype, name_spec, &name_repair_opts, &error_arg, error_call);

  FREE(1);
  return out;
}

r_obj* vec_c(
  r_obj* xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* name_repair,
  struct vctrs_arg* p_error_arg,
  struct r_lazy error_call
) {
  return list_combine_for_vec_c(
    xs,
    ptype,
    name_spec,
    name_repair,
    p_error_arg,
    error_call
  );
}
