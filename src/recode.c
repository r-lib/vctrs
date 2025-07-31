#include "vctrs.h"
#include "vec-int.h"
#include "optional.h"

#include "decl/recode-decl.h"

static
r_obj* vec_recode_values(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_obj* default_,
  enum list_unchop_unmatched unmatched,
  bool multiple_from,
  bool multiple_to,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_from_arg,
  struct vctrs_arg* p_to_arg,
  struct vctrs_arg* p_default_arg,
  r_obj* ptype,
  struct r_lazy call
) {
  int n_prot = 0;

  const bool has_default = default_ != r_null;

  // Vector checks ----

  obj_check_vector(x, p_x_arg, call);

  if (multiple_from) {
    obj_check_list(from, p_from_arg, call);
    list_check_all_vectors(from, p_from_arg, call);
  } else {
    obj_check_vector(from, p_from_arg, call);
  }

  if (multiple_to) {
    obj_check_list(to, p_to_arg, call);
    list_check_all_vectors(to, p_to_arg, call);
  } else {
    obj_check_vector(to, p_to_arg, call);
  }

  if (has_default) {
    obj_check_vector(default_, p_default_arg, call);
  }

  // Type checks ----

  // Finalize `ptype` up front
  // Remember that `default` gets a chance to participate in `ptype` determination
  ptype = KEEP_N(ptype_finalize(ptype, to, default_, multiple_to, p_to_arg, p_default_arg, call), &n_prot);

  // Cast `to` to the determined common type
  // Errors can happen here if user supplied `ptype`, short circuiting common type checks
  if (multiple_to) {
    to = KEEP_N(vec_cast_common(to, ptype, p_to_arg, call), &n_prot);
  } else {
    to = KEEP_N(vec_cast(to, ptype, p_to_arg, vec_args.empty, call), &n_prot);
  }

  // Cast `default` to determined common type
  if (has_default) {
    default_ = KEEP_N(vec_cast(default_, ptype, p_default_arg, vec_args.empty, call), &n_prot);
  }

  // Cast `from` to the type of `x`
  if (multiple_from) {
    // TODO!: `vec_cast_common()` should take `to_arg` so we can use `p_x_arg` here
    from = KEEP_N(vec_cast_common(from, x, p_from_arg, call), &n_prot);
  } else {
    from = KEEP_N(vec_cast(from, x, p_from_arg, p_x_arg, call), &n_prot);
  }

  // Size checks ----

  const r_ssize x_size = vec_size(x);
  r_ssize from_size = vec_size(from);
  const r_ssize to_size = vec_size(to);
  const r_ssize default_size = vec_size(default_);

  if (multiple_to) {
    // If list `to` is provided, strictly check against size of `from`, no recycling
    // Each element of `to` should be recyclable against `x`
    vec_check_size(to, from_size, p_to_arg, call);
    list_check_all_recyclable(to, x_size, p_to_arg, call);
  } else {
    // Check size of vector `to` as recyclable against the size of `from` (regardless of `multiple_from`)
    vec_check_recyclable(to, from_size, p_to_arg, call);
  }

  if (has_default && default_size != 1) {
    vec_check_size(default_, x_size, p_default_arg, call);
  }

  // Try `multiple_to` optimization
  if (multiple_to) {
    const struct vec_error_opts to_error_opts = {
      .p_arg = p_to_arg,
      .call = call
    };
    r_obj* to_sizes = KEEP_N(list_sizes(to, &to_error_opts), &n_prot);

    if (r_int_all(to_sizes, 1)) {
      // Optimize `to` to a flat vector and drop any names on the list, we've
      // already done casting checks and we don't want them on the flat form. We
      // don't expect any errors here.
      multiple_to = false;

      to = KEEP_N(vec_set_names(to, r_null), &n_prot);
      to = KEEP_N(
        vec_c(
          to,
          ptype,
          r_null,
          p_no_repair_opts,
          vec_args.empty,
          r_lazy_null
        ),
        &n_prot
      );
    }
  }

  // Try `multiple_from` optimization
  // (After `multiple_to` optimization, so we can double optimize)
  if (multiple_from && !multiple_to) {
    // Optimize `from` to a flat vector by repeating `to` elements to meet the
    // sizes of each `from` element. Drop names on the list form on `from` at
    // this point, we've done casting checks and no longer need them.
    multiple_from = false;

    const struct vec_error_opts from_error_opts = {
      .p_arg = p_from_arg,
      .call = call
    };
    r_obj* from_sizes = KEEP_N(list_sizes(from, &from_error_opts), &n_prot);

    from = KEEP_N(vec_set_names(from, r_null), &n_prot);
    from = KEEP_N(
      vec_c(
        from,
        x,
        r_null,
        p_no_repair_opts,
        vec_args.empty,
        r_lazy_null
      ),
      &n_prot
    );

    if (!r_int_all(from_sizes, 1)) {
      from_size = vec_size(from);
      to = KEEP_N(
        vec_rep_each(to, from_sizes, r_lazy_null, vec_args.empty, vec_args.empty),
        &n_prot
      );
    }
  }

  r_obj* components = r_null;
  if (multiple_to) {
    components = list_unchop_setup_with_multiple_to(
      x,
      from,
      to,
      x_size,
      from_size,
      to_size,
      multiple_from
    );
  } else {
    if (multiple_from) {
      r_stop_internal("Optimizations take care of the `multiple_from`, `!multiple_to` case");
    }

    components = list_unchop_setup_with_single_to(
      x,
      from,
      to,
      x_size,
      to_size
    );
  }
  KEEP_N(components, &n_prot);

  r_obj* xs = r_list_get(components, 0);
  r_obj* indices = r_list_get(components, 1);

  // TODO!: Catch `unmatched` error and rethrow as error mentioning `x`,
  // and with new class, but let other errors through
  r_obj* out = list_unchop(
    xs,
    indices,
    default_,
    ptype,
    new_optional_r_ssize(x_size),
    unmatched,
    r_null,
    p_no_repair_opts,
    vec_args.empty,
    vec_args.empty,
    call
  );

  FREE(n_prot);
  return out;
}

r_obj* ffi_vec_recode_values(
  r_obj* ffi_x,
  r_obj* ffi_from,
  r_obj* ffi_to,
  r_obj* ffi_default,
  r_obj* ffi_unmatched,
  r_obj* ffi_multiple_from,
  r_obj* ffi_multiple_to,
  r_obj* ffi_ptype,
  r_obj* ffi_frame
) {
  const bool multiple_from = r_arg_as_bool(ffi_multiple_from, "multiple_from");
  const bool multiple_to = r_arg_as_bool(ffi_multiple_to, "multiple_to");

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy from_arg_lazy = { .x = syms.from_arg, .env = ffi_frame };
  struct vctrs_arg from_arg = new_lazy_arg(&from_arg_lazy);

  struct r_lazy to_arg_lazy = { .x = syms.to_arg, .env = ffi_frame };
  struct vctrs_arg to_arg = new_lazy_arg(&to_arg_lazy);

  struct r_lazy default_arg_lazy = { .x = syms.default_arg, .env = ffi_frame };
  struct vctrs_arg default_arg = new_lazy_arg(&default_arg_lazy);

  struct r_lazy call = { .x = r_syms.call, .env = ffi_frame };

  enum list_unchop_unmatched unmatched = parse_unmatched(ffi_unmatched, call);

  return vec_recode_values(
    ffi_x,
    ffi_from,
    ffi_to,
    ffi_default,
    unmatched,
    multiple_from,
    multiple_to,
    &x_arg,
    &from_arg,
    &to_arg,
    &default_arg,
    ffi_ptype,
    call
  );
}

// Expect that all type and size checks have been done,
// we don't expect any errors while running setup code
static
r_obj* list_unchop_setup_with_multiple_to(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_ssize x_size,
  r_ssize from_size,
  r_ssize to_size,
  bool multiple_from
) {
  r_obj* out = KEEP(r_alloc_list(2));

  // This might still be the user's `to` list, and we are going to modify it
  to = r_clone_referenced(to);
  r_list_poke(out, 0, to);

  // We definitely want to drop names off the `to` list, as `to`
  // becomes `xs` and the `to` list names shouldn't pass through
  to = vec_set_names(to, r_null);
  r_list_poke(out, 0, to);

  r_obj* indices = build_indices(x, from, x_size, from_size, multiple_from);
  r_list_poke(out, 1, indices);

  r_obj* const* v_to = r_list_cbegin(to);
  r_obj* const* v_indices = r_list_cbegin(indices);

  for (r_ssize i = 0; i < to_size; ++i) {
    r_obj* elt = v_to[i];
    r_obj* index = v_indices[i];

    if (vec_size(elt) != 1) {
      // Slice `to` (same size as `x`) "down" to represent the utilized locations
      elt = vec_slice_unsafe(elt, index);
      r_list_poke(to, i, elt);
    }
  }

  FREE(1);
  return out;
}

// Expect that all type and size checks have been done,
// we don't expect any errors while running setup code
static
r_obj* list_unchop_setup_with_single_to(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_ssize x_size,
  r_ssize to_size
) {
  int n_prot = 0;

  // Find locations where `to` will be utilized
  // Want `na_equal = true`, which is the default, for `vec_recode_values(x, from = NA, to = to)`
  r_obj* loc = KEEP_N(vec_match(x, from), &n_prot);

  r_obj* loc_of_match = KEEP_N(r_int_locate_complete(loc), &n_prot);

  if (to_size != 1) {
    // Slice `to` (same size as `from`) to represent the utilized locations
    r_obj* loc_at_match = KEEP_N(vec_slice_unsafe(loc, loc_of_match), &n_prot);
    to = KEEP_N(vec_slice_unsafe(to, loc_at_match), &n_prot);
  }

  r_obj* out = KEEP_N(r_alloc_list(2), &n_prot);

  r_obj* xs = r_alloc_list(1);
  r_list_poke(out, 0, xs);
  r_list_poke(xs, 0, to);

  r_obj* indices = r_alloc_list(1);
  r_list_poke(out, 1, indices);
  r_list_poke(indices, 0, loc_of_match);

  FREE(n_prot);
  return out;
}

static
r_obj* build_indices(r_obj* x, r_obj* from, r_ssize x_size, r_ssize from_size, bool multiple_from) {
  int n_prot = 0;

  const r_ssize out_size = from_size;

  r_obj* loc_map = NULL;
  r_ssize* v_loc_map = NULL;

  if (multiple_from) {
    // Flatten `from`, tracking the `from_sizes`.
    // These allow us to map each match into the correct location bucket.
    const struct vec_error_opts from_error_opts = {
      .p_arg = vec_args.empty,
      .call = r_lazy_null
    };
    r_obj* from_sizes = KEEP_N(list_sizes(from, &from_error_opts), &n_prot);
    const int* v_from_sizes = r_int_cbegin(from_sizes);

    from = KEEP_N(vec_set_names(from, r_null), &n_prot);
    from = KEEP_N(
      vec_c(
        from,
        x,
        r_null,
        p_no_repair_opts,
        vec_args.empty,
        r_lazy_null
      ),
      &n_prot
    );

    from_size = vec_size(from);

    loc_map = KEEP_N(r_alloc_raw(from_size * sizeof(r_ssize)), &n_prot);
    v_loc_map = r_raw_begin(loc_map);
    r_ssize k = 0;

    for (r_ssize i = 0; i < out_size; ++i) {
      r_ssize elt_size = (r_ssize) v_from_sizes[i];
      for (r_ssize j = 0; j < elt_size; ++j) {
        v_loc_map[k++] = i;
      }
    }
  }

  // Find `x` values in `from`
  r_obj* loc = KEEP_N(vec_match(x, from), &n_prot);
  const int* v_loc = r_int_cbegin(loc);

  r_obj* out = KEEP_N(r_alloc_list(out_size), &n_prot);

  // Direct pointer to the `r_dyn_array`s we store in `out`
  struct r_dyn_array** p_out = (struct r_dyn_array**) R_alloc(out_size, sizeof(struct r_dyn_array*));

  // Assume `x` distributes evenly across the output, but don't let it get too low
  r_ssize initial_capacity = floor(r_ssize_as_double(x_size) / r_ssize_as_double(out_size));
  initial_capacity = R_MAX(initial_capacity, 16);

  for (r_ssize i = 0; i < out_size; ++i) {
    struct r_dyn_array* p_elt = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    r_list_poke(out, i, p_elt->shelter);
    p_out[i] = p_elt;
  }

  for (r_ssize i = 0; i < x_size; ++i) {
    int loc = v_loc[i];

    if (loc != r_globals.na_int) {
      loc = multiple_from ? v_loc_map[loc - 1] : loc - 1;
      r_dyn_int_push_back(p_out[loc], i + 1);
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    r_list_poke(out, i, r_dyn_unwrap(p_out[i]));
  }

  FREE(n_prot);
  return out;
}

static
r_obj* ptype_finalize(
  r_obj* ptype,
  r_obj* to,
  r_obj* default_,
  bool multiple_to,
  struct vctrs_arg* p_to_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy call
) {
  if (ptype != r_null) {
    // Performs scalar checks and whatnot
    return vec_ptype(ptype, vec_args.ptype, call);
  }

  // Otherwise `ptype` is `NULL`, and we determine it from `to` and `default`

  if (multiple_to) {
    // Use only `to` and `p_to_arg` first for best errors
    ptype = KEEP(vec_ptype_common_params(
      to,
      r_null,
      S3_FALLBACK_DEFAULT,
      p_to_arg,
      call
    ));

    // Now incorporate `default` and `p_default_arg`
    int _;
    ptype = KEEP(vec_ptype2_params(default_, ptype, p_default_arg, vec_args.empty, call, &_));

    FREE(2);
  } else {
    int _;
    ptype = KEEP(vec_ptype2_params(to, default_, p_to_arg, p_default_arg, call, &_));
    FREE(1);
  }

  return ptype;
}
