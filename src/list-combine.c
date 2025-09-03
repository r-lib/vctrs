#include "list-combine.h"
#include "vctrs.h"

#include "decl/list-combine-decl.h"

r_obj* ffi_list_combine(
  r_obj* ffi_xs,
  r_obj* ffi_indices,
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

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  struct name_repair_opts name_repair_opts = new_name_repair_opts(
    ffi_name_repair,
    r_lazy_null,
    false,
    error_call
  );
  KEEP(name_repair_opts.shelter);

  struct list_combine_indices_info indices_info = {
    .indices = ffi_indices,
    .p_indices_arg = &indices_arg
  };

  r_obj* out = list_combine(
    ffi_xs,
    &indices_info,
    ffi_ptype,
    ffi_name_spec,
    &name_repair_opts,
    &xs_arg,
    error_call
  );

  FREE(1);
  return out;
}

r_obj* list_combine(
  r_obj* xs,
  const struct list_combine_indices_info* p_indices_info,
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

  return list_combine_with_fallback_opts(
    xs,
    p_indices_info,
    ptype,
    name_spec,
    p_name_repair_opts,
    p_xs_arg,
    error_call,
    fallback_opts
  );
}

static
r_obj* list_combine_with_fallback_opts(
  r_obj* xs,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call,
  const struct fallback_opts fallback_opts
) {
  // `NULL` pointer is our signal of "no `indices`", i.e. combine sequentially
  // like `vec_c()`
  const bool has_indices = p_indices_info != NULL;

  // Within a single `list_combine()` iteration, we get our own local
  // copy of `indices` info. We may modify this directly, and we don't want our
  // changes to cause upstream changes if we are being called recursively.
  struct list_combine_indices_info local_indices_info;
  struct list_combine_indices_info* p_local_indices_info;

  if (has_indices) {
    // Copy over all `indices` information into local struct we can modify
    local_indices_info = *p_indices_info;
    p_local_indices_info = &local_indices_info;
  } else {
    p_local_indices_info = NULL;
  }

  return list_combine_impl(
    xs,
    has_indices,
    p_local_indices_info,
    ptype,
    name_spec,
    p_name_repair_opts,
    p_xs_arg,
    error_call,
    fallback_opts
  );
}

/**
 * Actual implementation for `list_combine()`
 *
 * Note that this is the one place where `p_indices_info` is not `const`!
 * We may modify the info struct directly here, and we manage protection
 * of any R objects within it.
 */
static
r_obj* list_combine_impl(
  r_obj* xs,
  bool has_indices,
  struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call,
  const struct fallback_opts fallback_opts
) {
  int n_protect = 0;

  obj_check_list(xs, p_xs_arg, error_call);

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_ssize xs_size = vec_size(xs);

  if (has_indices) {
    // Apply size/type checking to `indices` before possibly early exiting from
    // having a `NULL` common type or needing to apply a fallback
    obj_check_list(
      p_indices_info->indices,
      p_indices_info->p_indices_arg,
      error_call
    );

    if (xs_size != vec_size(p_indices_info->indices)) {
      r_abort_lazy_call(
        error_call,
        "%s (size %" R_PRI_SSIZE ") and %s (size %" R_PRI_SSIZE ") must be lists of the same size.",
        vec_arg_format(p_xs_arg),
        xs_size,
        vec_arg_format(p_indices_info->p_indices_arg),
        vec_size(p_indices_info->indices)
      );
    }
  }

  const struct ptype_common_opts ptype_common_opts = {
    .p_arg = p_xs_arg,
    .call = error_call,
    .fallback = fallback_opts
  };

  ptype = KEEP_N(
    vec_ptype_common_opts(
      xs,
      ptype,
      &ptype_common_opts
    ),
    &n_protect
  );

  if (ptype == r_null) {
    FREE(n_protect);
    return r_null;
  }

  if (needs_list_combine_common_class_fallback(ptype)) {
    r_obj* out = list_combine_common_class_fallback(
      xs,
      has_indices,
      p_indices_info,
      ptype,
      name_spec,
      p_name_repair_opts,
      p_xs_arg,
      error_call
    );
    FREE(n_protect);
    return out;
  }

  if (needs_list_combine_homogeneous_fallback(xs, ptype)) {
    r_obj* out = list_combine_homogeneous_fallback(
      xs,
      has_indices,
      p_indices_info,
      name_spec,
      p_xs_arg,
      error_call
    );
    FREE(n_protect);
    return out;
  }

  const bool assign_names = !r_inherits(name_spec, "rlang_zap");
  r_obj* xs_names = KEEP_N(r_names(xs), &n_protect);
  const bool xs_is_named = xs_names != r_null && !is_data_frame(ptype);

  // Sizes are reused by the sequential path when advancing the compact-seq index.
  // It's more efficient to build them once even though it requires an allocation.
  r_obj* xs_sizes = NULL;
  r_ssize* v_xs_sizes = NULL;

  r_ssize out_size = 0;

  if (has_indices) {
    // Infer size from `indices`
    // This happens before conversion to valid location vectors.
    out_size = compute_out_size_from_indices(p_indices_info->indices);
  } else {
    // Infer size from `xs`
    xs_sizes = KEEP_N(r_alloc_raw(xs_size * sizeof(r_ssize)), &n_protect);
    v_xs_sizes = r_raw_begin(xs_sizes);

    for (r_ssize i = 0; i < xs_size; ++i) {
      r_obj* x = v_xs[i];
      r_ssize size = vec_size(x);
      out_size += size;
      v_xs_sizes[i] = size;
    }
  }

  if (has_indices) {
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
    p_indices_info->indices = KEEP_N(
      list_as_locations(p_indices_info->indices, out_size, r_null),
      &n_protect
    );
  }

  r_keep_loc out_pi;
  r_obj* out = vec_init(ptype, out_size);
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
    .slice_value = ASSIGNMENT_SLICE_VALUE_no,
    .index_style = VCTRS_INDEX_STYLE_location,
    .assign_names = assign_names,
    .ignore_outer_names = true,
    .call = error_call
  };

  r_keep_loc out_names_pi;
  r_obj* out_names = r_null;
  KEEP_HERE(out_names, &out_names_pi);
  ++n_protect;

  r_ssize i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    p_xs_arg,
    xs_names,
    xs_size,
    &i
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

  for (; i < xs_size; ++i) {
    x = v_xs[i];

    if (x == r_null) {
      continue;
    }

    r_ssize index_size;

    // Advance `index`
    if (has_indices) {
      index = r_list_get(p_indices_info->indices, i);
      index_size = r_length(index);
    } else {
      index_size = v_xs_sizes[i];
      init_compact_seq(v_index, start, index_size, true);
    }

    // When we have `indices`, `x` should be size 1 or `index_size`
    //
    // When we don't have `indices`, we derive the index sizes from
    // `x` itself so there is no reason to check the size.
    //
    // We don't actually recycle `x` because both `vec_proxy_assign_opts()` and
    // `chr_assign()` efficiently recycle size 1 inputs, but we do check that
    // `x` is recyclable to the right size.
    if (has_indices) {
      vec_check_recyclable(x, index_size, p_x_arg, error_call);
    }

    // Handle optional names assignment
    if (assign_names) {
      r_obj* outer = xs_is_named ? r_chr_get(xs_names, i) : r_null;
      r_obj* inner = KEEP(vec_names(x));
      r_obj* x_names = KEEP(apply_name_spec(name_spec, outer, inner, index_size));

      if (x_names != r_null) {
        R_LAZY_ALLOC(out_names, out_names_pi, R_TYPE_character, out_size);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (x_names != chrs_empty) {
          out_names = chr_assign(
            out_names,
            index,
            x_names,
            VCTRS_OWNERSHIP_deep,
            ASSIGNMENT_SLICE_VALUE_no,
            VCTRS_INDEX_STYLE_location
          );
          KEEP_AT(out_names, out_names_pi);
        }
      }

      FREE(2);
    }

    cast_opts.x = x;
    x = vec_cast_opts(&cast_opts);
    KEEP_AT(x, x_pi);

    // Total ownership of `out` because it was freshly created with `vec_init()`
    out = vec_proxy_assign_opts(out, index, x, &proxy_assign_opts);
    KEEP_AT(out, out_pi);

    if (!has_indices) {
      start += index_size;
    }
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
      p_indices_info,
      ptype,
      name_spec,
      p_name_repair_opts,
      error_call,
      out_size
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
  const struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  r_obj* cls = KEEP(r_attrib_get(ptype, syms_fallback_class));
  bool implements_c = class_implements_base_c(cls);
  FREE(1);

  if (implements_c) {
    return base_list_combine_fallback(
      xs,
      has_indices,
      p_indices_info,
      name_spec,
      p_xs_arg,
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

    // Suboptimal: Call `list_combine_with_fallback_opts()` again to
    // combine vector with homogeneous class fallback
    return list_combine_with_fallback_opts(
      xs,
      p_indices_info,
      ptype,
      name_spec,
      p_name_repair_opts,
      p_xs_arg,
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
  const struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct r_lazy error_call,
  r_ssize n_rows
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

      df_list_combine_common_class_fallback(
        out_col,
        xs_col,
        has_indices,
        p_indices_info,
        ptype_col,
        name_spec,
        p_name_repair_opts,
        error_call,
        n_rows
      );

      FREE(1);
    } else if (needs_list_combine_common_class_fallback(ptype_col)) {
      r_obj* xs_col = KEEP(list_pluck(xs, i));

      r_obj* out_col = list_combine_common_class_fallback(
        xs_col,
        has_indices,
        p_indices_info,
        ptype_col,
        name_spec,
        p_name_repair_opts,
        vec_args.empty,
        error_call
      );
      r_list_poke(out, i, out_col);

      if (vec_size(out_col) != n_rows) {
        r_stop_internal(
          "`c()` method returned a vector of unexpected size %d instead of %d.",
          vec_size(out_col),
          n_rows
        );
      }

      // Remove fallback vector from the ptype so it doesn't get in
      // the way of restoration later on
      r_list_poke(ptype_orig, i, vec_ptype_final(out_col));

      FREE(1);
    }
  }

  FREE(n_protect);
}

// -------------------------------------------------------------------------------------------

static
bool needs_list_combine_homogeneous_fallback(r_obj* xs, r_obj* ptype) {
  if (!r_length(xs)) {
    return false;
  }

  r_obj* x = list_first_non_null(xs, NULL);
  if (!obj_is_vector(x)) {
    return false;
  }

  // Never fall back for `vctrs_vctr` classes to avoid infinite
  // recursion through `c.vctrs_vctr()`
  if (r_inherits(x, "vctrs_vctr")) {
    return false;
  }

  r_obj* x_class = KEEP(r_class(x));
  r_obj* ptype_class = KEEP(r_class(ptype));
  bool equal = equal_object(x_class, ptype_class);
  FREE(2);
  if (!equal) {
    return false;
  }

  return
    !vec_implements_ptype2(x) &&
    list_is_homogeneously_classed(xs) &&
    vec_implements_base_c(x);
}

// To perform homogeneous fallback, we invoke `c()` because we've
// checked in `needs_list_combine_homogeneous_fallback()` that
// this class does implement a `c()` method, so we trust it.
static
r_obj* list_combine_homogeneous_fallback(
  r_obj* xs,
  bool has_indices,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  return base_list_combine_fallback(
    xs,
    has_indices,
    p_indices_info,
    name_spec,
    p_xs_arg,
    error_call
  );
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
  const struct list_combine_indices_info* p_indices_info,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  if (!has_indices) {
    // Sequential combination, nothing fancy here
    return base_c_invoke(xs, name_spec, error_call);
  }

  // Otherwise we have `indices`, we are going to need to:
  // - Compute the `out_size` based on the indices
  // - Validate these indices with `list_as_locations()`
  // - Recycle each `xs` element to the size of the corresponding `indices` element
  // - Sequentially combine all `xs`
  // - Fallback slice the combined `xs` in an order determined by the `indices`
  //   to mimic direct assignment using the `indices`
  //
  // In the end we end up doing something like:
  //
  // ```
  // vec_slice_fallback(base_c(!!!xs), order(vec_c(!!!indices)))
  // ```

  // Pluck this out, we are going to modify `indices`. We avoid modifying
  // `p_indices_info->indices` directly, as other data frame columns might need
  // to use them in their original form.
  r_obj* indices = p_indices_info->indices;
  r_ssize out_size = compute_out_size_from_indices(indices);

  // Remember, `list_as_locations()` won't change the size of an individual
  // `index`, which is why we can compute the `out_size` from them.
  indices = KEEP(list_as_locations(indices, out_size, r_null));

  xs = KEEP(vec_recycle_xs_fallback(xs, indices, p_xs_arg, error_call));

  // Remove all `NULL`s from `xs` and their corresponding slot in `indices`.
  // Done after `out_size` computation and `indices` validation, same as main loop.
  // `base_c_invoke()` does this as well, but we need to remove the `indices` slot
  // at the same time.
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

  r_obj* index = KEEP(build_fallback_index(indices, out_size, error_call));

  out = vec_slice_fallback(out, index);

  FREE(6);
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
r_obj* build_fallback_index(r_obj* indices, r_ssize out_size, struct r_lazy error_call) {
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

  // Not necessarily same as `out_size`!
  //
  // ```
  // local_c_foobar()
  // list_combine(
  //   list(foobar("a"), NULL, foobar("b")),
  //   list(2, 3, 1)
  // )
  // ```
  //
  // Implies `out_size` of 3 but `NULL` causes the `3`
  // index to get dropped, so `index_size` is `2`.
  const r_ssize index_size = r_length(index);

  r_obj* locations = KEEP(r_alloc_integer(out_size));
  int* v_locations = r_int_begin(locations);

  // Initialize with missing to handle locations that are never selected
  for (r_ssize i = 0; i < out_size; ++i) {
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

// Sums the length of each `index` to compute the `out_size`
//
// Notably runs before `indices` is validated, but that's fine
// because `list_as_locations()` won't allow an index to change
// sizes.
static
r_ssize compute_out_size_from_indices(r_obj* indices) {
  r_ssize out_size = 0;

  r_obj* const* v_indices = r_list_cbegin(indices);
  r_ssize indices_size = r_length(indices);

  for (r_ssize i = 0; i < indices_size; ++i) {
    out_size += r_length(v_indices[i]);
  }

  return out_size;
}
