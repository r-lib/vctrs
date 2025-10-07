#include "vctrs.h"

#include "decl/list-unchop-decl.h"

r_obj* ffi_list_unchop(
  r_obj* x,
  r_obj* indices,
  r_obj* ptype,
  r_obj* name_spec,
  r_obj* name_repair,
  r_obj* frame
) {
  struct r_lazy error_arg_lazy = { .x = r_syms.error_arg, .env = frame };
  struct vctrs_arg error_arg = new_lazy_arg(&error_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = frame };

  struct name_repair_opts name_repair_opts = new_name_repair_opts(
    name_repair,
    r_lazy_null,
    false,
    error_call
  );
  KEEP(name_repair_opts.shelter);

  r_obj* out = list_unchop(
    x,
    indices,
    ptype,
    name_spec,
    &name_repair_opts,
    &error_arg,
    error_call
  );

  FREE(1);
  return out;
}

// `list_unchop()` is a thin wrapper around `list_combine()`
// with less options, but allows for `indices = NULL` to mean
// "sequential" `indices`, i.e. `vec_c()`.
//
// At the C level, use `vec_c()` directly for simple combinations,
// or `list_combine()` for more complex combinations.
static
r_obj* list_unchop(
  r_obj* xs,
  r_obj* indices,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* name_repair,
  struct vctrs_arg* p_error_arg,
  struct r_lazy error_call
) {
  return list_combine_for_list_unchop(
    xs,
    indices,
    ptype,
    name_spec,
    name_repair,
    p_error_arg,
    error_call
  );
}
