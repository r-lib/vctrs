#include "list-combine.h"
#include "vctrs.h"

#include "decl/list-combine-decl.h"

r_obj* ffi_list_combine(
  r_obj* ffi_xs,
  r_obj* ffi_indices,
  r_obj* ffi_size,
  r_obj* ffi_default,
  r_obj* ffi_unmatched,
  r_obj* ffi_multiple,
  r_obj* ffi_slice_xs,
  r_obj* ffi_ptype,
  r_obj* ffi_name_spec,
  r_obj* ffi_name_repair,
  r_obj* ffi_frame
) {
  // On the R side it's `x_arg` to go with `x`, but on the C side we use `xs`
  struct r_lazy xs_arg_lazy = { .x = r_syms.x_arg, .env = ffi_frame };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  struct r_lazy indices_arg_lazy = { .x = r_syms.indices_arg, .env = ffi_frame };
  struct vctrs_arg indices_arg = new_lazy_arg(&indices_arg_lazy);

  struct r_lazy default_arg_lazy = { .x = r_syms.default_arg, .env = ffi_frame };
  struct vctrs_arg default_arg = new_lazy_arg(&default_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");

  const enum list_combine_unmatched unmatched = parse_list_combine_unmatched(ffi_unmatched, error_call);
  const enum list_combine_multiple multiple = parse_list_combine_multiple(ffi_multiple, error_call);

  // On the R side it's `slice_x` to go with `x`, but on the C side we use `xs`
  const enum assignment_slice_value slice_xs =
    r_arg_as_bool(ffi_slice_xs, "slice_x") ?
    ASSIGNMENT_SLICE_VALUE_yes :
    ASSIGNMENT_SLICE_VALUE_no;

  struct name_repair_opts name_repair_opts = new_name_repair_opts(
    ffi_name_repair,
    r_lazy_null,
    false,
    error_call
  );
  KEEP(name_repair_opts.shelter);

  r_obj* out = list_combine(
    ffi_xs,
    ffi_indices,
    size,
    ffi_default,
    unmatched,
    multiple,
    slice_xs,
    ffi_ptype,
    ffi_name_spec,
    &name_repair_opts,
    &xs_arg,
    &indices_arg,
    &default_arg,
    error_call
  );

  FREE(1);
  return out;
}

r_obj* list_combine(
  r_obj* xs,
  r_obj* indices,
  r_ssize size,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_indices_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  const struct fallback_opts fallback_opts = {
    .s3 = r_is_true(r_peek_option("vctrs:::base_c_in_progress")) ?
      S3_FALLBACK_false :
      S3_FALLBACK_true
  };

  // `list_combine_impl()` supports `NULL` `indices` for `vec_c()` and
  // `list_unchop()`, which `list_combine()` does not, so we early check here
  // for that. This can technically be hit by users so we want a good error
  // message. `vec_c()` and `list_unchop()` use this to sequentially combine
  // `xs`, but `list_combine()` requires `indices` to be a list.
  obj_check_list(indices, p_indices_arg, error_call);

  const enum vctrs_index_style indices_style = compute_indices_style(indices, size);

  const bool has_indices = true;
  const bool has_default = default_ != r_null;

  return list_combine_impl(
    xs,
    has_indices,
    indices,
    indices_style,
    size,
    has_default,
    default_,
    unmatched,
    multiple,
    slice_xs,
    ptype,
    name_spec,
    p_name_repair_opts,
    p_xs_arg,
    p_indices_arg,
    p_default_arg,
    error_call,
    fallback_opts
  );
}

/**
 * `vec_c()` backport
 */
r_obj* list_combine_for_vec_c(
  r_obj* xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  r_obj* indices = r_null;

  return list_combine_for_list_unchop(
    xs,
    indices,
    ptype,
    name_spec,
    p_name_repair_opts,
    p_xs_arg,
    error_call
  );
}

/**
 * `list_unchop()` backport
 */
r_obj* list_combine_for_list_unchop(
  r_obj* xs,
  r_obj* indices,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  const struct fallback_opts fallback_opts = {
    .s3 = r_is_true(r_peek_option("vctrs:::base_c_in_progress")) ?
      S3_FALLBACK_false :
      S3_FALLBACK_true
  };

  bool has_indices = indices != r_null;
  struct vctrs_arg* p_indices_arg = vec_args.indices;

  // If `!has_indices`, `list_combine()` will compute the size from the sizes of
  // `xs` and will ignore whatever we put here.
  r_ssize size = 0;

  if (has_indices) {
    // Sums the length of each `index` to compute the `size`
    //
    // This was the way that `list_unchop()` would compute the output size when
    // `indices` were provided. In `list_combine()`, the `size` is explicitly
    // required to account for a few edge cases and to work well with `default`.
    //
    // Note that `list_as_locations()` in `list_combine()` isn't allowed to
    // change the `index` size, which is the only reason this works from a
    // theoretical point of view.
    obj_check_list(indices, p_indices_arg, error_call);

    r_obj* const* v_indices = r_list_cbegin(indices);
    r_ssize indices_size = vec_size(indices);

    for (r_ssize i = 0; i < indices_size; ++i) {
      size += r_length(v_indices[i]);
    }
  }

  bool has_default = false;
  r_obj* default_ = r_null;
  struct vctrs_arg* p_default_arg = vec_args.empty;

  enum list_combine_unmatched unmatched = LIST_COMBINE_UNMATCHED_default;
  enum list_combine_multiple multiple = LIST_COMBINE_MULTIPLE_last;
  const enum assignment_slice_value slice_xs = ASSIGNMENT_SLICE_VALUE_no;
  const enum vctrs_index_style indices_style = VCTRS_INDEX_STYLE_location;

  r_obj* out = KEEP(list_combine_impl(
    xs,
    has_indices,
    indices,
    indices_style,
    size,
    has_default,
    default_,
    unmatched,
    multiple,
    slice_xs,
    ptype,
    name_spec,
    p_name_repair_opts,
    p_xs_arg,
    p_indices_arg,
    p_default_arg,
    error_call,
    fallback_opts
  ));

  if (vec_is_unspecified(out) && r_is_object(out)) {
    // The following `list_c()` and `list_unchop()` cases historically return
    // `NULL` because they don't have a `size` argument to maintain an invariant
    // for. `list_combine()` returns `unspecified` for these, because there
    // could be a `size > 0` argument supplied and we need to retain the size
    // invariant. We rectify the difference here.
    //
    // ```
    // vec_c()
    // vec_c(NULL)
    // list_unchop(list())
    // list_unchop(list(), indices = list())
    // list_unchop(list(NULL))
    // list_unchop(list(NULL), indices = list(integer()))
    // ```
    //
    // This is an ambiguous edge case that we've currently defined as also
    // returning `NULL`. `list_combine()` returns `unspecified[2]` here, but
    // that's clearer because `size` has to be explicitly provided.
    //
    // ```
    // list_unchop(list(NULL), indices = list(1:2))
    // ```
    //
    // We still want these cases to return `NA` even though they are technically
    // "unspecified" outputs, so we explicitly check if the output is an S3 object
    // as well, i.e. its an explicit `"vctrs_unspecified"` and not just a logical
    // vector of `NA`s.
    //
    // ```
    // vec_c(NA)
    // list_unchop(list(NA), indices = list(1))
    // ```
    out = r_null;
  }

  FREE(1);
  return out;
}

/**
 * Actual implementation for `list_combine()`
 *
 * Exposes `fallback_opts` here for use in the fallback
 */
static
r_obj* list_combine_impl(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_indices_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call,
  const struct fallback_opts fallback_opts
) {
  int n_protect = 0;

  obj_check_list(xs, p_xs_arg, error_call);

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_ssize xs_size = vec_size(xs);

  // This is impossible with the exposed API, but let's sanity check
  if (!has_indices) {
    switch (multiple) {
    case LIST_COMBINE_MULTIPLE_last: break;
    case LIST_COMBINE_MULTIPLE_first: r_stop_internal("`multiple = 'first'` can't be set with sequential combination."); break;
    default: r_stop_unreachable();
    }
  }

  if (has_indices) {
    // Apply size/type checking to `indices` before possibly early exiting from
    // needing to apply a fallback
    obj_check_list(indices, p_indices_arg, error_call);
    vec_check_size(indices, xs_size, p_indices_arg, error_call);
  }

  // Sizes are reused by the sequential path when advancing the compact-seq index.
  // It's more efficient to build them once even though it requires an allocation.
  r_obj* xs_sizes = NULL;
  r_ssize* v_xs_sizes = NULL;

  if (!has_indices) {
    // Infer `size` from `xs` for `vec_c()` and `list_unchop(indices = NULL)`
    // sequential approach
    size = 0;

    xs_sizes = KEEP_N(r_alloc_raw(xs_size * sizeof(r_ssize)), &n_protect);
    v_xs_sizes = r_raw_begin(xs_sizes);

    for (r_ssize i = 0; i < xs_size; ++i) {
      r_obj* x = v_xs[i];
      r_ssize x_size = vec_size(x);
      size += x_size;
      v_xs_sizes[i] = x_size;
    }
  }

  if (has_indices && indices_style == VCTRS_INDEX_STYLE_location) {
    // Validate and convert `indices` if they exist.
    //
    // Note that we don't allow an individual `index` vector to change size
    // during validation. This is the only reason we can "infer" the output
    // size from the sum of the lengths of the `indices` before doing validation
    // and conversion. In particular:
    // - `NULL` indices become `integer()` (both size `0`).
    // - We don't allow character or logical indices.
    // - We don't allow negative or zero indices (these change the size).
    // - We don't allow oob indices (makes no sense since we inferred the size from lengths).
    // - Numeric `NA` propagates.
    //
    // There is nothing to validate for condition indices, they are logical vectors
    // where all 3 possible values are handled and we've already checked their sizes
    // match `size` in `compute_indices_style()`.
    indices = KEEP_N(list_as_locations(indices, size, r_null), &n_protect);
  }

  // Perform `unmatched` check
  // (before fallback cases!)
  switch (unmatched) {
  case LIST_COMBINE_UNMATCHED_default: {
    // Will use `default` in unmatched locations
    break;
  }
  case LIST_COMBINE_UNMATCHED_error: {
    if (default_ != r_null) {
      r_abort("Can't set `default` when `unmatched = \"error\"`.");
    }
    if (!has_indices) {
      r_stop_internal("`indices` should have been required if `unmatched` was set.");
    }
    check_any_unmatched(indices, indices_style, size, error_call);
    break;
  }
  default: {
    r_stop_unreachable();
  }
  }

  ptype = KEEP_N(
    ptype_common_with_default(
      ptype,
      xs,
      has_default,
      default_,
      p_xs_arg,
      p_default_arg,
      error_call,
      fallback_opts
    ),
    &n_protect
  );

  if (needs_list_combine_common_class_fallback(ptype)) {
    r_obj* out = list_combine_common_class_fallback(
      xs,
      has_indices,
      indices,
      indices_style,
      size,
      has_default,
      default_,
      multiple,
      slice_xs,
      ptype,
      name_spec,
      p_name_repair_opts,
      p_xs_arg,
      p_indices_arg,
      p_default_arg,
      error_call
    );
    FREE(n_protect);
    return out;
  }

  if (needs_list_combine_homogeneous_fallback(xs, has_default, default_, ptype)) {
    r_obj* out = list_combine_homogeneous_fallback(
      xs,
      has_indices,
      indices,
      indices_style,
      size,
      has_default,
      default_,
      multiple,
      slice_xs,
      name_spec,
      p_xs_arg,
      p_default_arg,
      error_call
    );
    FREE(n_protect);
    return out;
  }

  if (ptype == r_null) {
    // Even when there are no inputs and we can't determine a `ptype`, the user
    // will have supplied a `size`, so as an invariant we should return
    // something with this `size`. Our size preserving identity type is
    // `<unspecified[size]>`, so we use that.
    //
    // We catch `<unspecified[0]>` in `vec_c()` and `list_unchop()` and return
    // `NULL` in those cases instead, because that is what they have historically
    // returned and you can't supply `size` there, so this only happens in the
    // empty input cases of those functions.
    //
    // Assuming `NULL` is roughly equivalent to `unspecified(0)`, this gives us:
    //
    // ```
    // vec_c()
    // #> NULL
    // list_unchop(list(), indices = list())
    // #> NULL
    // list_combine(list(), indices = list(), size = 0)
    // #> unspecified[0] # Consistent with size != 0 case.
    // list_combine(list(), indices = list(), size = 5)
    // #> unspecified[5] # Preserves size, good.
    // ```
    //
    // The most theoretically correct thing may be to return `unspecified(0)`
    // from `vec_c()` and `list_unchop()` as well, but we make a concious effort
    // to avoid exposing this type to users when we can. Since those functions
    // don't have the `size` invariant issue, `NULL` seems to be a good
    // alternative.
    // https://github.com/r-lib/vctrs/issues/1980
    ptype = vctrs_shared_empty_uns;
  }

  const bool assign_names = !r_inherits(name_spec, "rlang_zap");
  r_obj* xs_names = KEEP_N(r_names(xs), &n_protect);
  const bool xs_is_named = xs_names != r_null && !is_data_frame(ptype);

  r_keep_loc out_pi;
  r_obj* out = vec_init(ptype, size);
  KEEP_HERE(out, &out_pi);
  ++n_protect;

  out = vec_proxy_recurse(out);
  KEEP_AT(out, out_pi);

  // - We own the `proxy` container
  // - We own `proxy` recursively
  // - We call `vec_proxy_recurse()` so must restore recursively
  const struct vec_restore_opts restore_opts = {
    .ownership = VCTRS_OWNERSHIP_deep,
    .recursively_proxied = true
  };
 const struct vec_proxy_assign_opts proxy_assign_opts = {
    .ownership = VCTRS_OWNERSHIP_deep,
    .recursively_proxied = true,
    .slice_value = slice_xs,
    .index_style = indices_style,
    .assign_names = assign_names,
    .ignore_outer_names = true,
    .call = error_call
  };

  r_keep_loc out_names_pi;
  r_obj* out_names = r_null;
  KEEP_HERE(out_names, &out_names_pi);
  ++n_protect;

  r_ssize xs_i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    p_xs_arg,
    xs_names,
    xs_size,
    &xs_i
  );
  KEEP_N(p_x_arg->shelter, &n_protect);

  struct cast_opts cast_opts = {
    .to = ptype,
    .p_x_arg = p_x_arg,
    .call = error_call,
    .fallback = fallback_opts
  };

  r_keep_loc x_pi;
  r_obj* x = r_null;
  KEEP_HERE(x, &x_pi);
  ++n_protect;

  r_keep_loc index_pi;
  r_obj* index = r_null;
  KEEP_HERE(index, &index_pi);
  ++n_protect;

  // For the sequential path
  r_ssize start = 0;
  int* v_index = NULL;

  if (!has_indices) {
    // Sequential path reuses the same compact sequence `index`
    index = compact_seq(0, 0, true);
    KEEP_AT(index, index_pi);
    v_index = r_int_begin(index);
  }

  for (r_ssize i = 0; i < xs_size; ++i) {
    switch (multiple) {
    case LIST_COMBINE_MULTIPLE_last: xs_i = i; break;
    case LIST_COMBINE_MULTIPLE_first: xs_i = xs_size - 1 - i; break;
    default: r_stop_unreachable();
    }

    x = v_xs[xs_i];

    if (x == r_null) {
      continue;
    }

    r_ssize index_size;

    // Advance `index`
    if (has_indices) {
      index = r_list_get(indices, xs_i);
      index_size = r_length(index);
    } else {
      index_size = v_xs_sizes[xs_i];
      init_compact_seq(v_index, start, index_size, true);
    }

    // When we have `indices`, `x`'s size must be compatible with the `index`'s
    // size. This is dependent on `slice_xs` and `indices_style`.
    //
    // When we don't have `indices`, we derive the index sizes from
    // `x` itself so there is no reason to recheck the size.
    //
    // We don't actually recycle `x` because both `vec_proxy_assign_opts()` and
    // `chr_assign()` efficiently recycle size 1 inputs.
    if (has_indices) {
      check_recyclable_against_index(
        x,
        index,
        size,
        slice_xs,
        indices_style,
        p_x_arg,
        error_call
      );
    }

    // Handle optional names assignment
    if (assign_names) {
      r_obj* outer = xs_is_named ? r_chr_get(xs_names, xs_i) : r_null;
      r_obj* inner = KEEP(vec_names(x));
      r_obj* x_names = KEEP(apply_name_spec(name_spec, outer, inner, index_size));

      if (x_names != r_null) {
        R_LAZY_ALLOC(out_names, out_names_pi, R_TYPE_character, size);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (x_names != chrs_empty) {
          out_names = chr_assign(
            out_names,
            index,
            x_names,
            VCTRS_OWNERSHIP_deep,
            slice_xs,
            indices_style
          );
          KEEP_AT(out_names, out_names_pi);
        }
      }

      FREE(2);
    }

    cast_opts.x = x;
    x = vec_cast_opts(&cast_opts);
    KEEP_AT(x, x_pi);

    out = vec_proxy_assign_opts(out, index, x, &proxy_assign_opts);
    KEEP_AT(out, out_pi);

    if (!has_indices) {
      start += index_size;
    }
  }

  if (has_default) {
    // `default` uses a slightly modified form of `proxy_assign_opts` and `cast_opts`
    // - `default` is size 1 or size of the output, so uses `ASSIGNMENT_SLICE_VALUE_yes`.
    // - `default`'s index is always built using a logical vector, so uses `VCTRS_INDEX_STYLE_condition`.
    // - `default` has its own special `p_default_arg`.
    struct vec_proxy_assign_opts default_proxy_assign_opts = proxy_assign_opts;
    default_proxy_assign_opts.index_style = VCTRS_INDEX_STYLE_condition;
    default_proxy_assign_opts.slice_value = ASSIGNMENT_SLICE_VALUE_yes;

    struct cast_opts default_cast_opts = cast_opts;
    default_cast_opts.p_x_arg = p_default_arg;

    // Compute `default` condition index
    index = compute_default_index(indices, indices_style, size);
    KEEP_AT(index, index_pi);

    // `default` recycles against the output size, not the `index`
    vec_check_recyclable(default_, size, p_default_arg, error_call);

    // Handle optional names assignment
    if (assign_names) {
      // `outer` names don't exist, but `name_spec` could still `zap()` any `inner` names
      r_obj* outer = r_null;
      r_obj* inner = KEEP(vec_names(default_));
      r_obj* x_names = KEEP(apply_name_spec(name_spec, outer, inner, size));

      if (x_names != r_null) {
        R_LAZY_ALLOC(out_names, out_names_pi, R_TYPE_character, size);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (x_names != chrs_empty) {
          out_names = chr_assign(
            out_names,
            index,
            x_names,
            VCTRS_OWNERSHIP_deep,
            ASSIGNMENT_SLICE_VALUE_yes,
            VCTRS_INDEX_STYLE_condition
          );
          KEEP_AT(out_names, out_names_pi);
        }
      }

      FREE(2);
    }

    default_cast_opts.x = default_;
    x = vec_cast_opts(&default_cast_opts);
    KEEP_AT(x, x_pi);

    out = vec_proxy_assign_opts(out, index, x, &default_proxy_assign_opts);
    KEEP_AT(out, out_pi);
  }

  if (
    fallback_opts.s3 == S3_FALLBACK_true &&
      is_data_frame(out) &&
      needs_df_list_combine_common_class_fallback(out)
  ) {
    // Perform the common class fallback on any columns of the
    // data frame that require it
    df_list_combine_common_class_fallback(
      out,
      xs,
      has_indices,
      indices,
      indices_style,
      size,
      has_default,
      default_,
      multiple,
      slice_xs,
      ptype,
      name_spec,
      p_name_repair_opts,
      p_indices_arg,
      error_call
    );
  }

  out = vec_restore_opts(out, ptype, &restore_opts);
  KEEP_AT(out, out_pi);

  if (out_names != r_null) {
    out_names = KEEP(vec_as_names(out_names, p_name_repair_opts));
    out = vec_set_names(out, out_names);
    FREE(1);
  } else if (!assign_names) {
    // FIXME: `vec_ptype2()` doesn't consistently zaps names, so `out`
    // might have been initialised with names. This branch can be
    // removed once #1020 is resolved.
    out = vec_set_names(out, r_null);
  }

  FREE(n_protect);
  return out;
}

// -------------------------------------------------------------------------------------------

static
bool needs_list_combine_common_class_fallback(r_obj* ptype) {
  if (!vec_is_common_class_fallback(ptype)) {
    return false;
  }

  // Suboptimal: Prevent infinite recursion through `vctrs_vctr` method
  r_obj* cls = r_attrib_get(ptype, syms_fallback_class);
  cls = r_chr_get(cls, r_length(cls) - 1);

  return cls != strings_vctrs_vctr;
}

// Common class fallback combination method
//
// If it doesn't look like we know how to handle a class,
// we still provide a fallback approach in some cases.
//
// `vec_ptype_common()` knows to return a special "common type" `ptype` object
// in these cases, which is then detectable by `vec_is_common_class_fallback()`.
//
// Attached to that `ptype` is the class name of interest that we are
// working with.
//
// - If that class has a `c()` method, we invoke it.
//
// - Otherwise, we try `list_combine()` again, this time without the
//   ability to fallback. This only works if our other fallback case is hit,
//   which is when every object in the list is of a homogenous type, in which
//   case we again call `c()` if that homogenous type has a `c()` method, or we
//   fall through and let `list_combine()` try to run and push the
//   homogenous attributes onto the final output.
static
r_obj* list_combine_common_class_fallback(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_indices_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  r_obj* cls = KEEP(r_attrib_get(ptype, syms_fallback_class));
  bool implements_c = class_implements_base_c(cls);
  FREE(1);

  if (implements_c) {
    return base_list_combine_fallback(
      xs,
      has_indices,
      indices,
      indices_style,
      size,
      has_default,
      default_,
      multiple,
      slice_xs,
      name_spec,
      p_xs_arg,
      p_default_arg,
      error_call
    );
  } else {
    struct fallback_opts fallback_opts = {
      .s3 = S3_FALLBACK_false
    };
    struct ptype_common_opts ptype_common_opts = {
      .p_arg = p_xs_arg,
      .call = error_call,
      .fallback = fallback_opts
    };

    // Throw out the `vctrs:::common_class_fallback` ptype,
    // it's served its purpose by getting us here
    ptype = r_null;

    // Should cause a common type error, unless another fallback
    // kicks in (for instance, homogeneous class with homogeneous
    // attributes)
    vec_ptype_common_opts(xs, ptype, &ptype_common_opts);

    // We will have already checked `unmatched` before the fallback
    // is invoked, so no need to check it again
    enum list_combine_unmatched unmatched = LIST_COMBINE_UNMATCHED_default;

    // Suboptimal: Call `list_combine_impl()` again to
    // combine vector with homogeneous class fallback
    return list_combine_impl(
      xs,
      has_indices,
      indices,
      indices_style,
      size,
      has_default,
      default_,
      unmatched,
      multiple,
      slice_xs,
      ptype,
      name_spec,
      p_name_repair_opts,
      p_xs_arg,
      p_indices_arg,
      p_default_arg,
      error_call,
      fallback_opts
    );
  }
}

// -------------------------------------------------------------------------------------------

// To check if a data frame needs common class fallback treatment, we
// recursively look through the columns for `vec_is_common_class_fallback()` to
// be `true` on any column.
bool needs_df_list_combine_common_class_fallback(r_obj* x) {
  r_ssize n_cols = r_length(x);
  r_obj* const * v_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = v_x[i];

    if (vec_is_common_class_fallback(col)) {
      return true;
    }
    if (is_data_frame(col) && needs_df_list_combine_common_class_fallback(col)) {
      return true;
    }
  }

  return false;
}

// If a column of a data frame requires common class treatment, then it has not
// actually been assigned into `out` yet. We pluck out the relevant column from
// each element of `xs` and perform a common class fallback combination approach
// on that column to form the final column, then push it into place in `out`.
void df_list_combine_common_class_fallback(
  r_obj* out,
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_indices_arg,
  struct r_lazy error_call
) {
  int n_protect = 0;
  r_ssize n_cols = r_length(out);

  r_obj* ptype_orig = ptype;

  if (!is_data_frame(ptype)) {
    ptype = KEEP_N(vec_proxy(ptype), &n_protect);
    if (!is_data_frame(ptype)) {
      r_stop_internal("Expected fallback target to have a df proxy.");
    }
  }

  if (r_length(ptype) != n_cols ||
      r_typeof(out) != R_TYPE_list ||
      r_typeof(ptype) != R_TYPE_list) {
    r_stop_internal("`ptype` and `out` must be lists of the same length.");
  }

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = r_list_get(out, i);
    r_obj* ptype_col = r_list_get(ptype, i);

    if (is_data_frame(col) && needs_df_list_combine_common_class_fallback(ptype_col)) {
      // Recurse into df-cols
      r_obj* out_col = r_list_get(out, i);
      r_obj* xs_col = KEEP(list_pluck(xs, i));
      r_obj* default_col = has_default ? r_list_get(default_, i) : r_null;

      df_list_combine_common_class_fallback(
        out_col,
        xs_col,
        has_indices,
        indices,
        indices_style,
        size,
        has_default,
        default_col,
        multiple,
        slice_xs,
        ptype_col,
        name_spec,
        p_name_repair_opts,
        p_indices_arg,
        error_call
      );

      FREE(1);
    } else if (needs_list_combine_common_class_fallback(ptype_col)) {
      r_obj* xs_col = KEEP(list_pluck(xs, i));
      r_obj* default_col = has_default ? r_list_get(default_, i) : r_null;

      struct vctrs_arg* p_xs_col_arg = vec_args.empty;
      struct vctrs_arg* p_default_col_arg = vec_args.empty;

      r_obj* out_col = list_combine_common_class_fallback(
        xs_col,
        has_indices,
        indices,
        indices_style,
        size,
        has_default,
        default_col,
        multiple,
        slice_xs,
        ptype_col,
        name_spec,
        p_name_repair_opts,
        p_xs_col_arg,
        p_indices_arg,
        p_default_col_arg,
        error_call
      );
      r_list_poke(out, i, out_col);

      if (vec_size(out_col) != size) {
        r_stop_internal(
          "`c()` method returned a vector of unexpected size %d instead of %d.",
          vec_size(out_col),
          size
        );
      }

      // Remove fallback vector from the ptype so it doesn't get in
      // the way of restoration later on
      r_list_poke(ptype_orig, i, vec_ptype_final(out_col, vec_args.empty, error_call));

      FREE(1);
    }
  }

  FREE(n_protect);
}

// -------------------------------------------------------------------------------------------

static
bool needs_list_combine_homogeneous_fallback(
  r_obj* xs,
  bool has_default,
  r_obj* default_,
  r_obj* ptype
) {
  r_obj* first = list_first_non_null(xs, NULL);

  if (first == r_null && has_default) {
    // i.e. `list_combine(x = list(), default = foobar(1))`
    first = default_;
  }

  if (!obj_is_vector(first)) {
    return false;
  }

  // Never fall back for `vctrs_vctr` classes to avoid infinite
  // recursion through `c.vctrs_vctr()`
  if (r_inherits(first, "vctrs_vctr")) {
    return false;
  }

  r_obj* ptype_class = KEEP(r_class(ptype));

  if (!obj_has_class(first, ptype_class)) {
    // Cheap test before consulting `vec_ptype2()` and `c()` methods
    FREE(1);
    return false;
  }

  bool out = !vec_implements_ptype2(first) &&
    list_all_have_class(xs, ptype_class) &&
    (has_default ? obj_has_class(default_, ptype_class) : true) &&
    vec_implements_base_c(first);

  FREE(1);
  return out;
}

// To perform homogeneous fallback, we invoke `c()` because we've
// checked in `needs_list_combine_homogeneous_fallback()` that
// this class does implement a `c()` method, so we trust it.
static
r_obj* list_combine_homogeneous_fallback(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  return base_list_combine_fallback(
    xs,
    has_indices,
    indices,
    indices_style,
    size,
    has_default,
    default_,
    multiple,
    slice_xs,
    name_spec,
    p_xs_arg,
    p_default_arg,
    error_call
  );
}

static
bool list_all_have_class(r_obj* xs, r_obj* class) {
  r_ssize size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* x = v_xs[i];

    if (x == r_null) {
      // Allow `NULL`s
      continue;
    }

    if (!obj_has_class(x, class)) {
      return false;
    }
  }

  return true;
}

static
bool obj_has_class(r_obj* x, r_obj* class) {
  r_obj* x_class = KEEP(r_class(x));
  bool out = equal_object(x_class, class);
  FREE(1);
  return out;
}

// -------------------------------------------------------------------------------------------

/**
 * Core routine for R level list combine fallback
 *
 * If `indices` aren't involved, this just calls out R level `c()` to combine
 * the `xs` in order, with some special handling for `NULL` and `unspecified`.
 *
 * If `indices` are involved, we have to recreate some of the behavior we get in
 * the "main" path, like computing the total output size and recycling `xs` to
 * each `index` size. We still use `c()` to combine sequentially, but then we
 * reslice the combined results to put them in the order specified by the
 * `indices`.
 */
static
r_obj* base_list_combine_fallback(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  if (!has_indices) {
    // Sequential combination, nothing fancy here
    return base_c_invoke(xs, name_spec, error_call);
  }

  // Otherwise we have `indices`. We need to recreate a bunch of the "main" path
  // logic, and then combine all `xs` together and reorder using the `indices`.
  //
  // We end up doing something like:
  //
  // ```
  // vec_slice_fallback(base_c(!!!xs), order(vec_c(!!!indices)))
  // ```

  // Normalize `indices` to the location style, because that's what the fallback
  // is designed to handle. It's also the style we convert `default` to.
  switch (indices_style) {
  case VCTRS_INDEX_STYLE_location: {
    // Nothing to do, these are the "normalized" style used for the fallback
    break;
  }
  case VCTRS_INDEX_STYLE_condition: {
    indices_style = VCTRS_INDEX_STYLE_location;
    indices = list_condition_to_location_indices(indices);
    break;
  }
  default: r_stop_unreachable();
  }
  KEEP(indices);

  // Normalize and check `xs` sizes
  //
  // - If `slice_xs = no`, each `x` must be size 1 or the size of the `index`
  //   - Size 1 must be recycled up to the size of the `index`
  // - If `slice_xs = yes`, each `x` must be size 1 or size `size`
  //   - Size 1 must be recycled up to the size of the `index`
  //   - Size `size` must be sliced down to the size of the `index`
  switch (slice_xs) {
  case ASSIGNMENT_SLICE_VALUE_no: {
    xs = vec_recycle_xs_fallback(xs, indices, p_xs_arg, error_call);
    break;
  }
  case ASSIGNMENT_SLICE_VALUE_yes: {
    xs = vec_slice_xs_fallback(xs, indices, size, p_xs_arg, error_call);
    break;
  }
  default: r_stop_unreachable();
  }
  KEEP(xs);

  // Reverse `xs` and `indices` if required for `multiple`
  //
  // - Done after recycling/slicing of `xs` because `p_xs_arg` is used there
  //   and we need to generate correct index locations in errors.
  // - Done before `default` handling because `default` is always pushed
  //   at the end.
  switch (multiple) {
  case LIST_COMBINE_MULTIPLE_last: {
    // Nothing to do, this is the standard behavior
    break;
  }
  case LIST_COMBINE_MULTIPLE_first: {
    xs = KEEP(vec_reverse(xs));
    indices = KEEP(vec_reverse(indices));
    FREE(2);
    break;
  }
  }
  KEEP(xs);
  KEEP(indices);

  if (has_default) {
    // Materialize the `default`'s index in location style, as that is what
    // we normalized `indices` to.
    r_obj* default_index = KEEP(compute_default_index(indices, indices_style, size));
    default_index = KEEP(r_lgl_which(default_index, false));

    // `default` recycles against the output size
    const r_ssize default_size = vec_size(default_);
    const r_ssize default_index_size = r_length(default_index);

    // Other `xs` have been sliced already, we now need to sliced `default`,
    // which is always provided in `slice_xs = yes` style.
    if (default_size == 1) {
      // Recycle "up" to the size of the index
      default_ = vec_recycle_fallback(default_, default_index_size, p_default_arg, error_call);
    } else if (default_size == size) {
      // Slice "down" to the size of the index
      default_ = vec_slice_fallback(default_, default_index);
    } else {
      // `default` is the wrong size, error
      vec_check_recyclable(default_, size, p_default_arg, error_call);
    }
    KEEP(default_);

    // Append the default to `xs` and `indices` before the fallback
    xs = KEEP(push_default(xs, default_));
    indices = KEEP(push_default_index(indices, default_index));

    FREE(5);
  }
  KEEP(xs);
  KEEP(indices);

  // Remove all `NULL`s from `xs` and their corresponding slot in `indices`.
  //
  // `base_c_invoke()` does this as well, but we need to remove the `indices` slot
  // at the same time.
  //
  // - Done after `default_index` computation, so `default_index` doesn't capture
  //   dropped indices.
  // - Done after `vec_recycle_xs_fallback()` so we have correct indices in recycling
  //   error messages.
  if (vec_any_missing(xs)) {
    r_obj* complete = KEEP(vec_detect_complete(xs));
    complete = KEEP(r_lgl_which(complete, false));
    xs = KEEP(vec_slice_unsafe(xs, complete));
    indices = KEEP(vec_slice_unsafe(indices, complete));
    FREE(4);
  }
  KEEP(xs);
  KEEP(indices);

  r_obj* out = KEEP(base_c_invoke(xs, name_spec, error_call));

  r_obj* index = KEEP(build_fallback_index(indices, size, error_call));

  out = vec_slice_fallback(out, index);

  FREE(10);
  return out;
}

static
r_obj* base_c_invoke(
  r_obj* xs,
  r_obj* name_spec,
  struct r_lazy error_call
) {
  if (vctrs_debug_verbose) {
    r_obj* x = list_first_non_null(xs, NULL);
    r_printf(
      "Falling back to `base::c()` for class `%s`.\n",
      r_chr_get_c_string(r_class(x), 0)
    );
  }

  if (name_spec_is_inner(name_spec)) {
    // We don't support most `name_spec` options in the fallback,
    // but we do allow this one because it is extremely useful
    // and easy to implement
    name_spec = r_null;

    if (r_names(xs) != r_null) {
      // Remove outer names, but remember we likely don't own `xs`!
      xs = KEEP(r_clone_referenced(xs));
      r_attrib_poke_names(xs, r_null);
      FREE(1);
    }
  }
  KEEP(xs);

  if (name_spec != r_null) {
    stop_name_spec_in_fallback(xs, error_call);
  }

  r_obj* ffi_call = KEEP(r_call2(r_sym("base_c_invoke"), xs));
  r_obj* out = r_eval(ffi_call, vctrs_ns_env);

  FREE(2);
  return out;
}

static
void stop_name_spec_in_fallback(r_obj* xs, struct r_lazy error_call) {
  r_obj* common_class = KEEP(r_class(list_first_non_null(xs, NULL)));
  const char* class_str = r_chr_get_c_string(common_class, 0);

  r_abort_lazy_call(
    error_call,
    "Can't use a name specification with non-vctrs types.\n"
    "vctrs methods must be implemented for class `%s`.\n"
    "See <https://vctrs.r-lib.org/articles/s3-vector.html>.",
    class_str
  );
}

static
r_obj* push_default(
  r_obj* xs,
  r_obj* default_
) {
  const r_ssize xs_size = vec_size(xs);

  r_obj* xs_names = KEEP(r_names(xs));
  xs = KEEP(r_list_resize(xs, xs_size + 1));
  r_list_poke(xs, xs_size, default_);

  if (xs_names != r_null) {
    xs_names = r_chr_resize(xs_names, xs_size + 1);
    r_attrib_poke_names(xs, xs_names);
    r_chr_poke(xs_names, xs_size, r_strs.empty);
  }

  FREE(2);
  return xs;
}

static
r_obj* push_default_index(
  r_obj* indices,
  r_obj* default_index
) {
  const r_ssize indices_size = vec_size(indices);

  r_obj* indices_names = KEEP(r_names(indices));
  indices = KEEP(r_list_resize(indices, indices_size + 1));
  r_list_poke(indices, indices_size, default_index);

  if (indices_names != r_null) {
    indices_names = r_chr_resize(indices_names, indices_size + 1);
    r_attrib_poke_names(indices, indices_names);
    r_chr_poke(indices_names, indices_size, r_strs.empty);
  }

  FREE(2);
  return indices;
}

static
r_obj* build_fallback_index(r_obj* indices, r_ssize size, struct r_lazy error_call) {
  const struct name_repair_opts name_repair_opts = {
    .type = NAME_REPAIR_none,
    .fn = r_null,
    .call = error_call
  };

  r_obj* index = KEEP(vec_c(
    indices,
    r_globals.empty_int,
    r_null,
    &name_repair_opts,
    vec_args.indices,
    error_call
  ));

  const int* v_index = r_int_cbegin(index);

  // Not necessarily same as `size`!
  //
  // ```
  // local_c_foobar()
  // list_combine(
  //   list(foobar("a"), NULL, foobar("b")),
  //   indices = list(2, 3, 1),
  //   size = 3
  // )
  // ```
  //
  // Implies `size` of 3 but `NULL` causes the `3`
  // index to get dropped, so `index_size` is `2`.
  const r_ssize index_size = r_length(index);

  r_obj* locations = KEEP(r_alloc_integer(size));
  int* v_locations = r_int_begin(locations);

  // Initialize with missing to handle locations that are never selected
  for (r_ssize i = 0; i < size; ++i) {
    v_locations[i] = r_globals.na_int;
  }

  // At each index location, put the order value, note that results in "last
  // wins" behavior when multiple indices overwrite the same location
  for (r_ssize i = 0; i < index_size; ++i) {
    const int elt = v_index[i];
    if (elt != r_globals.na_int) {
      v_locations[elt - 1] = i + 1;
    }
  }

  FREE(2);
  return locations;
}

/**
 * Recycles each element of `xs` to match the size
 * of the corresponding `indices` index.
 *
 * Used for `slice_xs = no`.
 */
static
r_obj* vec_recycle_xs_fallback(
  r_obj* xs,
  r_obj* indices,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  r_ssize xs_size = vec_size(xs);
  r_obj* xs_names = r_names(xs);
  xs = KEEP(r_clone_referenced(xs));

  r_ssize i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    p_xs_arg,
    xs_names,
    xs_size,
    &i
  );
  KEEP(p_x_arg->shelter);

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* const* v_indices = r_list_cbegin(indices);

  for (; i < xs_size; ++i) {
    r_obj* x = v_xs[i];
    r_ssize index_size = r_length(v_indices[i]);
    r_list_poke(xs, i, vec_recycle_fallback(x, index_size, p_x_arg, error_call));
  }

  FREE(2);
  return xs;
}

/**
 * Slices each element of `xs` to match the size
 * of the corresponding `indices` index.
 *
 * Used for `slice_xs = yes`.
 */
static
r_obj* vec_slice_xs_fallback(
  r_obj* xs,
  r_obj* indices,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  r_ssize xs_size = vec_size(xs);
  r_obj* xs_names = r_names(xs);
  xs = KEEP(r_clone_referenced(xs));

  r_ssize i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    p_xs_arg,
    xs_names,
    xs_size,
    &i
  );
  KEEP(p_x_arg->shelter);

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* const* v_indices = r_list_cbegin(indices);

  for (; i < xs_size; ++i) {
    r_obj* x = v_xs[i];
    r_obj* index = v_indices[i];

    const r_ssize x_size = vec_size(x);
    const r_ssize index_size = r_length(index);

    if (x_size == 1) {
      // Recycle "up" to the size of the index
      x = vec_recycle_fallback(x, index_size, p_x_arg, error_call);
    } else if (x_size == size) {
      // Slice "down" to the size of the index
      x = vec_slice_fallback(x, index);
    } else {
      // `x` is the wrong size, error
      vec_check_recyclable(x, size, p_x_arg, error_call);
    }

    r_list_poke(xs, i, x);
  }

  FREE(2);
  return xs;
}

// Converts `VCTRS_INDEX_STYLE_condition` indices to
// `VCTRS_INDEX_STYLE_location` indices for the fallback.
static
r_obj* list_condition_to_location_indices(r_obj* indices) {
  // We probably don't own these, the user provides them.
  indices = KEEP(r_clone_referenced(indices));

  const r_ssize indices_size = r_length(indices);
  r_obj* const* v_indices = r_list_cbegin(indices);

  // Because we want `c(FALSE, NA, TRUE)` to become `c(NA, TRUE)`
  // for assignment and size checking purposes
  const bool na_propagate = true;

  for (r_ssize i = 0; i < indices_size; ++i) {
    r_obj* index = v_indices[i];
    r_list_poke(indices, i, r_lgl_which(index, na_propagate));
  }

  FREE(1);
  return indices;
}

// Determines if the vector `x` implements an S3/S4 method for the `c()` generic
static
bool vec_implements_base_c(r_obj* x) {
  if (!r_is_object(x)) {
    return false;
  }

  if (IS_S4_OBJECT(x)) {
    return s4_find_method(x, s4_c_method_table) != r_null;
  } else {
    return s3_find_method("c", x, base_method_table) != r_null;
  }
}

// Determines if the class vector `cls` implements an S3/S4 method for the `c()` generic
// (inheritance is taken into account)
static
bool class_implements_base_c(r_obj* cls) {
  if (s3_class_find_method("c", cls, base_method_table) != r_null) {
    return true;
  }
  if (s4_class_find_method(cls, s4_c_method_table) != r_null) {
    return true;
  }
  return false;
}

// -------------------------------------------------------------------------------------------

enum list_combine_unmatched parse_list_combine_unmatched(r_obj* unmatched, struct r_lazy error_call) {
  if (!r_is_string(unmatched)) {
    r_stop_internal("`unmatched` must be a string.");
  }

  const char* c_unmatched = r_chr_get_c_string(unmatched, 0);

  if (!strcmp(c_unmatched, "default")) return LIST_COMBINE_UNMATCHED_default;
  if (!strcmp(c_unmatched, "error")) return LIST_COMBINE_UNMATCHED_error;

  r_abort_lazy_call(
    error_call,
    "`unmatched` must be either \"default\" or \"error\"."
  );
}

enum list_combine_multiple parse_list_combine_multiple(r_obj* multiple, struct r_lazy error_call) {
  if (!r_is_string(multiple)) {
    r_stop_internal("`multiple` must be a string.");
  }

  const char* c_multiple = r_chr_get_c_string(multiple, 0);

  if (!strcmp(c_multiple, "last")) return LIST_COMBINE_MULTIPLE_last;
  if (!strcmp(c_multiple, "first")) return LIST_COMBINE_MULTIPLE_first;

  r_abort_lazy_call(
    error_call,
    "`multiple` must be either \"last\" or \"first\"."
  );
}

// -------------------------------------------------------------------------------------------

/**
 * Compute the `indices` index style
 *
 * The index style is "all or nothing" for simplicity.
 *
 * - If all index vectors are simple logical condition index vectors, we use
 *   `VCTRS_INDEX_STYLE_condition`.
 * - Otherwise we use `VCTRS_INDEX_STYLE_location`, which then goes through
 *   `list_as_locations()` which requires that all index vectors be positive
 *   integer location vectors to begin with.
 */
static
enum vctrs_index_style compute_indices_style(r_obj* indices, r_ssize size) {
  r_obj* const* v_indices = r_list_cbegin(indices);
  r_ssize indices_size = vec_size(indices);

  for (r_ssize i = 0; i < indices_size; ++i) {
    r_obj* index = v_indices[i];

    if (!is_condition_index(index, size)) {
      return VCTRS_INDEX_STYLE_location;
    }
  }

  return VCTRS_INDEX_STYLE_condition;
}

// -------------------------------------------------------------------------------------------

static
void check_any_unmatched(
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  struct r_lazy error_call
) {
  r_obj* default_index = KEEP(compute_default_index(indices, indices_style, size));

  if (r_lgl_any(default_index)) {
    r_obj* loc = KEEP(r_lgl_which(default_index, false));
    stop_combine_unmatched(loc, error_call);
  }

  FREE(1);
}

static
void stop_combine_unmatched(r_obj* loc, struct r_lazy error_call) {
  r_obj* syms[3] = {
    syms_loc,
    syms_call,
    NULL
  };
  r_obj* args[3] = {
    loc,
    KEEP(r_lazy_eval_protect(error_call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_combine_unmatched, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_combine_unmatched");
}

static
r_obj* compute_default_index(
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size
) {
  const r_ssize indices_size = r_length(indices);
  r_obj* const* v_indices = r_list_cbegin(indices);

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  // Initialize mark everything as unmatched
  r_p_lgl_fill(v_out, 1, size);

  // Unmark matched locations according to the index style
  switch (indices_style) {
  case VCTRS_INDEX_STYLE_location: {
    for (r_ssize i = 0; i < indices_size; ++i) {
      r_obj* index = v_indices[i];
      const r_ssize index_size = r_length(index);
      const int* v_index = r_int_cbegin(index);

      for (r_ssize j = 0; j < index_size; ++j) {
        const int elt = v_index[j];

        if (elt != r_globals.na_int) {
          // If not `NA`, mark location as matched by at least 1 index
          v_out[elt - 1] = 0;
        }
      }
    }
    break;
  }
  case VCTRS_INDEX_STYLE_condition: {
    for (r_ssize i = 0; i < indices_size; ++i) {
      r_obj* index = v_indices[i];
      const int* v_index = r_lgl_cbegin(index);

      for (r_ssize j = 0; j < size; ++j) {
        const int elt = v_index[j];
        // If `TRUE`, mark location as matched by at least 1 index.
        // Specially optimized to be branchless, which does greatly help.
        v_out[j] *= (elt != 1);
      }
    }
    break;
  }
  default: {
    r_stop_unreachable();
  }
  }

  FREE(1);
  return out;
}

// -------------------------------------------------------------------------------------------

// `ptype` determination is complicated by the fact that both `xs` and `default`
// will contribute to the output type, and we want the best error messages
// possible. We can't just fold `default` into `xs` because we don't get
// a chance to use `p_default_arg`. We could materialize `p_default_arg`
// and use it as a name on the `xs` list, but that means it will get combined
// with `p_xs_arg` when there is an error, which we don't want.
static
r_obj* ptype_common_with_default(
  r_obj* ptype,
  r_obj* xs,
  bool has_default,
  r_obj* default_,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call,
  const struct fallback_opts fallback_opts
) {
  if (ptype != r_null) {
    // Performs scalar checks and whatnot
    return vec_ptype_final(ptype, vec_args.ptype, error_call);
  }

  // Okay `ptype` is `NULL`. We determine it from `xs` and `default`.

  const struct ptype_common_opts ptype_common_opts = {
    .p_arg = p_xs_arg,
    .call = error_call,
    .fallback = fallback_opts
  };

  // Use only `xs` and `p_xs_arg` first for best errors
  ptype = KEEP(vec_ptype_common_opts(
    xs,
    ptype,
    &ptype_common_opts
  ));

  // Now incorporate `default` and `p_default_arg` if required
  if (has_default) {
    const struct ptype2_opts ptype2_opts = {
      .x = ptype,
      .y = default_,
      .p_x_arg = vec_args.empty,
      .p_y_arg = p_default_arg,
      .call = error_call,
      .fallback = fallback_opts
    };
    int _;
    ptype = vec_ptype2_opts(&ptype2_opts, &_);
  }
  KEEP(ptype);

  FREE(2);
  return ptype;
}

// -------------------------------------------------------------------------------------------
