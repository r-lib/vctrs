#include "vctrs.h"
#include "vec-bool.h"
#include "optional.h"

#include "decl/case-when-decl.h"

static
r_obj* vec_case_when(
  r_obj* cases,
  r_obj* values,
  r_obj* default_,
  enum list_unchop_unmatched unmatched,
  r_obj* ptype,
  struct optional_r_ssize size,
  struct vctrs_arg* p_cases_arg,
  struct vctrs_arg* p_values_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy call
) {
  obj_check_list(cases, p_cases_arg, call);
  list_check_all_vectors(cases, p_cases_arg, call);

  obj_check_list(values, p_values_arg, call);
  list_check_all_vectors(values, p_values_arg, call);

  r_obj* const* v_cases = r_list_cbegin(cases);
  const r_ssize cases_size = r_length(cases);
  r_obj* cases_names = r_names(cases);

  // We need to poke back into `values` as we resize each `value`, so we need to own the list,
  // and we want to keep the names on `values` for error purposes in `list_unchop()`
  values = KEEP(r_clone_referenced(values));

  r_obj* const* v_values = r_list_cbegin(values);
  const r_ssize values_size = r_length(values);
  r_obj* values_names = r_names(values);

  if (cases_size != values_size) {
    r_abort_lazy_call(
      call,
      "The number of supplied cases (%" R_PRIdXLEN_T ") must equal the number of supplied values (%" R_PRIdXLEN_T ").",
      cases_size,
      values_size
    );
  }

  const r_ssize out_size = compute_size(size, cases, cases_size, call);

  // No recycling on `cases`
  list_check_all_size(cases, out_size, p_cases_arg, call);

  r_ssize cases_i = 0;
  struct vctrs_arg* p_case_arg = new_subscript_arg(
    p_cases_arg,
    cases_names,
    cases_size,
    &cases_i
  );
  KEEP(p_case_arg->shelter);

  // No common typing on `cases`.
  // Check logical type and no dim for all `cases`.
  for (r_ssize i = 0; i < cases_size; ++i) {
    r_obj* case_ = v_cases[i];
    cases_i = i;

    if (r_typeof(case_) != R_TYPE_logical) {
      r_abort_lazy_call(
        call,
        "%s must be a logical vector, not %s.",
        vec_arg_format(p_case_arg),
        r_obj_type_friendly(case_)
      );
    }

    if (has_dim(case_)) {
      r_abort_lazy_call(
        call,
        "%s can't be an array.",
        vec_arg_format(p_case_arg)
      );
    }
  }

  r_ssize value_i = 0;
  struct vctrs_arg* p_value_arg = new_subscript_arg(
    p_values_arg,
    values_names,
    values_size,
    &value_i
  );
  KEEP(p_value_arg->shelter);

  struct r_vector_bool* p_unused = r_new_vector_bool(out_size);
  KEEP(p_unused->shelter);
  r_vector_bool_fill(p_unused, true);
  bool* v_unused = p_unused->v_data;

  // Allocate `location` vector of maximum size that we can reuse across
  // all `case` vectors, this is particularly worth it when there are
  // many conditions, and is very cache friendly at the expense of more
  // memory than is strictly necessary
  r_obj* location_overallocated = KEEP(r_alloc_integer(out_size));
  int* v_location_overallocated = r_int_begin(location_overallocated);

  r_obj* locations = KEEP(r_alloc_list(values_size));

  // Translate logical `cases` into integer `locations`
  // - First `TRUE` wins
  // - `NA` treated as `FALSE`
  //
  // Use those `locations` to slice each `values` "down" as needed
  for (r_ssize i = 0; i < values_size; ++i) {
    r_obj* value = v_values[i];
    const r_ssize value_size = vec_size(value);
    value_i = i;

    r_obj* case_ = v_cases[i];
    const int* v_case = r_lgl_cbegin(case_);

    r_ssize location_size = 0;

    for (r_ssize j = 0; j < out_size; ++j) {
      const bool use = v_unused[j] && (v_case[j] == 1);

      // Update whether this location has been used or not for next time
      v_unused[j] &= !use;

      // Record the actual location index
      v_location_overallocated[location_size] = j + 1;
      location_size += use;
    }

    // Copy from `location_overallocated` into final `location`
    r_obj* location = r_alloc_integer(location_size);
    r_list_poke(locations, i, location);
    int* v_location = r_int_begin(location);
    memcpy(v_location, v_location_overallocated, location_size * sizeof(int));

    if (value_size != 1) {
      if (value_size == out_size) {
        r_list_poke(values, i, vec_slice_unsafe(value, location));
      } else {
        vec_check_recyclable(value, out_size, p_value_arg, call);
      }
    }
  }

  r_obj* out = list_unchop(
    values,
    locations,
    default_,
    ptype,
    new_optional_r_ssize(out_size),
    unmatched,
    name_spec_inner,
    p_no_repair_opts,
    p_values_arg,
    p_default_arg,
    call
  );

  FREE(6);
  return out;
}

static
r_obj* vec_replace_when(
  r_obj* x,
  r_obj* cases,
  r_obj* values,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_cases_arg,
  struct vctrs_arg* p_values_arg,
  struct r_lazy call
) {
  obj_check_vector(x, p_x_arg, call);

  r_obj* default_ = x;
  struct vctrs_arg* p_default_arg = p_x_arg;

  const enum list_unchop_unmatched unmatched = LIST_UNCHOP_UNMATCHED_default;
  r_obj* ptype = KEEP(vec_ptype_final(x));
  const struct optional_r_ssize size = new_optional_r_ssize(vec_size(x));

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
    call
  ));

  // `vec_case_when()` creates a new vector and names come from any of `values`
  // or `default`, but `vec_replace_when()` modifies an existing vector and
  // should act like `[<-` and `base::replace()`, retaining existing names.
  r_obj* names = KEEP(vec_names(x));
  // TODO: Ideally set `VCTRS_OWNED_true` here
  out = vec_set_names(out, names);

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

  struct r_lazy call = { .x = r_syms.call, .env = ffi_frame };

  const struct optional_r_ssize size = parse_size(ffi_size);
  const enum list_unchop_unmatched unmatched = parse_unmatched(ffi_unmatched, call);

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
    call
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

  struct r_lazy call = { .x = r_syms.call, .env = ffi_frame };

  return vec_replace_when(
    ffi_x,
    ffi_cases,
    ffi_values,
    &x_arg,
    &cases_arg,
    &values_arg,
    call
  );
}

// Figure out the output size
// - `size` if supplied
// - Size of 1st `cases` element if one exists
// - Size 0 otherwise
static
r_ssize compute_size(
  struct optional_r_ssize size,
  r_obj* cases,
  r_ssize cases_size,
  struct r_lazy call
) {
  if (optional_r_ssize_is_some(size)) {
    return optional_r_ssize_unwrap(size);
  }

  if (cases_size == 0) {
    return 0;
  }

  r_obj* case_ = r_list_get(cases, 0);
  return vec_size(case_);
}
