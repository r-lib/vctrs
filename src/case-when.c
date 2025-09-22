#include "vctrs.h"

#include "decl/case-when-decl.h"

r_obj* vec_case_when(
  r_obj* cases,
  r_obj* values,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  r_obj* ptype,
  r_ssize size,
  struct vctrs_arg* p_cases_arg,
  struct vctrs_arg* p_values_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  obj_check_list(cases, p_cases_arg, error_call);
  list_check_all_vectors(cases, p_cases_arg, error_call);

  obj_check_list(values, p_values_arg, error_call);
  list_check_all_vectors(values, p_values_arg, error_call);

  // Infer `size` from first element of `cases` unless specified.
  // We do this in `vec_case_when()` but not in `list_combine()`
  // because `vec_case_when()` only takes logical indices, which
  // has less ambiguity about the output size.
  size = compute_size(size, cases);

  // Enforce that all `cases` are condition indices
  // (bare logical vectors of the correct `size`)
  list_check_all_condition_indices(cases, size, p_cases_arg, error_call);

  const enum list_combine_multiple multiple = LIST_COMBINE_MULTIPLE_first;
  const enum assignment_slice_value slice_values = ASSIGNMENT_SLICE_VALUE_yes;

  return list_combine(
    values,
    cases,
    size,
    default_,
    unmatched,
    multiple,
    slice_values,
    ptype,
    name_spec_inner,
    p_no_repair_opts,
    p_values_arg,
    p_cases_arg,
    p_default_arg,
    error_call
  );
}

r_obj* vec_replace_when(
  r_obj* x,
  r_obj* cases,
  r_obj* values,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_cases_arg,
  struct vctrs_arg* p_values_arg,
  struct r_lazy error_call
) {
  obj_check_vector(x, p_x_arg, error_call);

  r_obj* default_ = x;
  struct vctrs_arg* p_default_arg = p_x_arg;

  const enum list_combine_unmatched unmatched = LIST_COMBINE_UNMATCHED_default;

  const r_ssize size = vec_size(x);
  r_obj* ptype = KEEP(vec_ptype_final(x, p_x_arg, error_call));

  r_obj* out = KEEP(vec_case_when(
    cases,
    values,
    default_,
    unmatched,
    ptype,
    size,
    p_cases_arg,
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
  r_obj* ffi_cases,
  r_obj* ffi_values,
  r_obj* ffi_default,
  r_obj* ffi_unmatched,
  r_obj* ffi_ptype,
  r_obj* ffi_size,
  r_obj* ffi_frame
) {
  struct r_lazy cases_arg_lazy = { .x = syms.cases_arg, .env = ffi_frame };
  struct vctrs_arg cases_arg = new_lazy_arg(&cases_arg_lazy);

  struct r_lazy values_arg_lazy = { .x = syms.values_arg, .env = ffi_frame };
  struct vctrs_arg values_arg = new_lazy_arg(&values_arg_lazy);

  struct r_lazy default_arg_lazy = { .x = syms.default_arg, .env = ffi_frame };
  struct vctrs_arg default_arg = new_lazy_arg(&default_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const r_ssize size = (ffi_size == r_null) ? -1 : r_arg_as_ssize(ffi_size, "size");
  const enum list_combine_unmatched unmatched = parse_list_combine_unmatched(ffi_unmatched, error_call);

  return vec_case_when(
    ffi_cases,
    ffi_values,
    ffi_default,
    unmatched,
    ffi_ptype,
    size,
    &cases_arg,
    &values_arg,
    &default_arg,
    error_call
  );
}

r_obj* ffi_vec_replace_when(
  r_obj* ffi_x,
  r_obj* ffi_cases,
  r_obj* ffi_values,
  r_obj* ffi_frame
) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy cases_arg_lazy = { .x = syms.cases_arg, .env = ffi_frame };
  struct vctrs_arg cases_arg = new_lazy_arg(&cases_arg_lazy);

  struct r_lazy values_arg_lazy = { .x = syms.values_arg, .env = ffi_frame };
  struct vctrs_arg values_arg = new_lazy_arg(&values_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  return vec_replace_when(
    ffi_x,
    ffi_cases,
    ffi_values,
    &x_arg,
    &cases_arg,
    &values_arg,
    error_call
  );
}

// Figure out the output size
// - `size` if supplied
// - Size of 1st `cases` element if one exists
// - Size 0 if `cases` is empty
static
r_ssize compute_size(r_ssize size, r_obj* cases) {
  if (size != -1) {
    return size;
  }

  if (r_length(cases) == 0) {
    return 0;
  }

  return r_length(r_list_get(cases, 0));
}
