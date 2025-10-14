#include "slice-interleave.h"
#include "vctrs.h"

#include "decl/slice-interleave-decl.h"

r_obj* ffi_list_interleave(
  r_obj* ffi_x,
  r_obj* ffi_size,
  r_obj* ffi_ptype,
  r_obj* ffi_name_spec,
  r_obj* ffi_name_repair,
  r_obj* ffi_frame
) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const r_ssize size = (ffi_size == r_null) ?
    -1 :
    vec_as_short_length(ffi_size, vec_args.size, error_call);

  struct name_repair_opts name_repair_opts = new_name_repair_opts(
    ffi_name_repair,
    r_lazy_null,
    false,
    error_call
  );
  KEEP(name_repair_opts.shelter);

  r_obj* out = list_interleave(
    ffi_x,
    size,
    ffi_ptype,
    ffi_name_spec,
    &name_repair_opts,
    &x_arg,
    error_call
  );

  FREE(1);
  return out;
}

r_obj* list_interleave(
  r_obj* x,
  r_ssize size,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_x_arg,
  struct r_lazy error_call
) {
  obj_check_list(x, p_x_arg, error_call);

  const r_ssize elt_size = (size == -1) ?
    vec_check_size_common(x, 0, p_x_arg, error_call) :
    size;

  r_obj* const* v_x = r_list_cbegin(x);
  const r_ssize x_size = r_length(x);

  // `x_size` excluding `NULL`s
  const r_ssize x_size_used = list_interleave_x_size_used(
    v_x,
    x_size
  );

  const r_ssize out_size = r_ssize_mult(x_size_used, elt_size);

  if (out_size > R_LEN_T_MAX) {
    r_abort(
      "Long vectors are not yet supported in `list_interleave()`. "
      "Result from interleaving would have size %td, which is larger "
      "than the maximum supported size of 2^31 - 1.",
      out_size
    );
  }

  r_obj* indices = KEEP(list_interleave_indices(
    v_x,
    x_size,
    x_size_used,
    elt_size
  ));

  r_obj* default_ = r_null;
  struct vctrs_arg* p_indices_arg = vec_args.empty;
  struct vctrs_arg* p_default_arg = vec_args.empty;

  r_obj* out = list_combine(
    x,
    indices,
    out_size,
    default_,
    LIST_COMBINE_UNMATCHED_default,
    LIST_COMBINE_MULTIPLE_last,
    ASSIGNMENT_SLICE_VALUE_no,
    ptype,
    name_spec,
    p_name_repair_opts,
    p_x_arg,
    p_indices_arg,
    p_default_arg,
    error_call
  );

  FREE(1);
  return out;
}

static
r_ssize list_interleave_x_size_used(r_obj* const* v_x, r_ssize x_size) {
  r_ssize x_size_used = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    r_obj* elt = v_x[i];

    if (elt == r_null) {
      continue;
    }

    ++x_size_used;
  }

  return x_size_used;
}

static
r_obj* list_interleave_indices(
  r_obj* const* v_x,
  r_ssize x_size,
  r_ssize x_size_used,
  r_ssize elt_size
) {
  r_obj* indices = KEEP(r_alloc_list(x_size));

  r_ssize start = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    r_obj* elt = v_x[i];

    if (elt == r_null) {
      // Insert `integer()` index for `NULL`, don't advance `start`
      r_list_poke(indices, i, r_globals.empty_int);
      continue;
    }

    ++start;

    r_obj* index = r_alloc_integer(elt_size);
    r_list_poke(indices, i, index);
    int* v_index = r_int_begin(index);

    for (r_ssize j = 0; j < elt_size; ++j) {
      v_index[j] = start + x_size_used * j;
    }
  }

  FREE(1);
  return indices;
}
