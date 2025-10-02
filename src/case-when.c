#include "vctrs.h"

#include "decl/case-when-decl.h"

r_obj* vec_case_when(
  r_obj* conditions,
  r_obj* values,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  r_obj* ptype,
  r_ssize size,
  struct vctrs_arg* p_conditions_arg,
  struct vctrs_arg* p_values_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  obj_check_list(conditions, p_conditions_arg, error_call);
  list_check_all_condition_indices(conditions, p_conditions_arg, error_call);

  obj_check_list(values, p_values_arg, error_call);
  list_check_all_vectors(values, VCTRS_ALLOW_NULL_no, p_values_arg, error_call);

  // Infer `size` from first element of `conditions` unless specified.
  // We do this in `vec_case_when()` but not in `list_combine()`
  // because `vec_case_when()` only takes logical indices, which
  // has less ambiguity about the output size.
  size = compute_size(size, conditions);
  list_check_all_size(conditions, size, VCTRS_ALLOW_NULL_no, p_conditions_arg, error_call);

  const enum list_combine_multiple multiple = LIST_COMBINE_MULTIPLE_first;
  const enum assignment_slice_value slice_values = ASSIGNMENT_SLICE_VALUE_yes;

  return list_combine(
    values,
    conditions,
    size,
    default_,
    unmatched,
    multiple,
    slice_values,
    ptype,
    name_spec_inner,
    p_no_repair_opts,
    p_values_arg,
    p_conditions_arg,
    p_default_arg,
    error_call
  );
}

r_obj* vec_replace_when(
  r_obj* x,
  r_obj* conditions,
  r_obj* values,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_conditions_arg,
  struct vctrs_arg* p_values_arg,
  struct r_lazy error_call
) {
  obj_check_vector(x, VCTRS_ALLOW_NULL_no, p_x_arg, error_call);

  r_obj* default_ = x;
  struct vctrs_arg* p_default_arg = p_x_arg;

  const enum list_combine_unmatched unmatched = LIST_COMBINE_UNMATCHED_default;

  const r_ssize size = vec_size(x);
  r_obj* ptype = KEEP(vec_ptype_final(x, p_x_arg, error_call));

  r_obj* out = KEEP(vec_case_when(
    conditions,
    values,
    default_,
    unmatched,
    ptype,
    size,
    p_conditions_arg,
    p_values_arg,
    p_default_arg,
    error_call
  ));

  // `vec_case_when()` creates a new vector and names come from any of `values`
  // or `default`, but `vec_replace_when()` modifies an existing vector and
  // should act like `[<-` and `base::replace()`, retaining existing names.
  // `out` is totally fresh, so we can claim deep ownership over it (though we
  // only require shallow ownership to set names).
  r_obj* names = KEEP(vec_names(x));
  out = vec_set_names(out, names, VCTRS_OWNERSHIP_deep);

  FREE(3);
  return out;
}

r_obj* ffi_vec_case_when(
  r_obj* ffi_conditions,
  r_obj* ffi_values,
  r_obj* ffi_default,
  r_obj* ffi_unmatched,
  r_obj* ffi_ptype,
  r_obj* ffi_size,
  r_obj* ffi_frame
) {
  struct r_lazy conditions_arg_lazy = { .x = syms.conditions_arg, .env = ffi_frame };
  struct vctrs_arg conditions_arg = new_lazy_arg(&conditions_arg_lazy);

  struct r_lazy values_arg_lazy = { .x = syms.values_arg, .env = ffi_frame };
  struct vctrs_arg values_arg = new_lazy_arg(&values_arg_lazy);

  struct r_lazy default_arg_lazy = { .x = syms.default_arg, .env = ffi_frame };
  struct vctrs_arg default_arg = new_lazy_arg(&default_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const r_ssize size = (ffi_size == r_null) ? -1 : r_arg_as_ssize(ffi_size, "size");
  const enum list_combine_unmatched unmatched = parse_list_combine_unmatched(ffi_unmatched, error_call);

  return vec_case_when(
    ffi_conditions,
    ffi_values,
    ffi_default,
    unmatched,
    ffi_ptype,
    size,
    &conditions_arg,
    &values_arg,
    &default_arg,
    error_call
  );
}

r_obj* ffi_vec_replace_when(
  r_obj* ffi_x,
  r_obj* ffi_conditions,
  r_obj* ffi_values,
  r_obj* ffi_frame
) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy conditions_arg_lazy = { .x = syms.conditions_arg, .env = ffi_frame };
  struct vctrs_arg conditions_arg = new_lazy_arg(&conditions_arg_lazy);

  struct r_lazy values_arg_lazy = { .x = syms.values_arg, .env = ffi_frame };
  struct vctrs_arg values_arg = new_lazy_arg(&values_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  return vec_replace_when(
    ffi_x,
    ffi_conditions,
    ffi_values,
    &x_arg,
    &conditions_arg,
    &values_arg,
    error_call
  );
}

// Figure out the output size
// - `size` if supplied
// - Size of 1st `conditions` element if one exists
// - Size 0 if `conditions` is empty
static
r_ssize compute_size(r_ssize size, r_obj* conditions) {
  if (size != -1) {
    return size;
  }

  if (r_length(conditions) == 0) {
    return 0;
  }

  return r_length(r_list_get(conditions, 0));
}
