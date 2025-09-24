#include "vctrs.h"
#include "type-data-frame.h"

/*
 * Index manager/generator for chopping purposes
 *
 * There are 3 types of possible indices:
 * - If `indices = NULL, sizes = NULL`, then we use a sequential size 1 index
 *   that just increments by 1 from `0` to `vec_size(x) - 1`.
 * - If `indices` is supplied, then each element of `indices` is an integer
 *   vector of locations to chop with.
 * - If `sizes` is supplied, then each element of `sizes` is the size of the
 *   current slice to chop. The sizes are accumulated in order to get the
 *   start location of the next slice.
 *
 * - Generate the next index with `indices_next()`.
 * - Generate the output size with `indices_out_size()`.
 *
 * @member shelter The shelter to protect the entire chop indices manager.
 * @member indices, v_indices
 *   - If `NULL`, then `indices` aren't being used. `v_indices` is set to
 *     `NULL`.
 *   - Otherwise, a list of integer vector indices to chop with. `v_indices` is
 *     set to `r_list_cbegin(indices)`.
 * @member sizes, v_sizes
 *   - If `NULL`, then `sizes` aren't being used. `v_sizes` is set to `NULL`.
 *   - Otherwise, an integer vector of sequential sizes to chop with. `v_sizes`
 *     is set to `r_int_cbegin(sizes)`.
 * @member index, p_index
 *   - If neither `indices` nor `sizes` are provided, `index` is a scalar
 *     integer vector that starts at 0 and is incremented by 1 at every
 *     iteration. `p_index` points to `r_int_begin(index)` and is used to
 *     perform the increment.
 *   - If `indices` is provided, this is set to the i-th element of `indices`
 *     at each iteration, and `p_index` is set to `NULL`.
 *   - If `sizes` is provided, this is a compact-seq representing the i-th
 *     slice. `p_index` points to `r_int_begin(index)` and is used to updated
 *     the compact-seq at each iteration.
 * @member has_indices Whether or not `indices` was provided.
 * @member has_sizes Whether or not `sizes` was provided.
 * @member loc The current iteration value.
 */
struct vctrs_chop_indices {
  r_obj* shelter;

  r_obj* indices;
  r_obj* const* v_indices;

  r_obj* sizes;
  const int* v_sizes;

  r_obj* index;
  int* p_index;

  bool has_indices;
  bool has_sizes;

  r_ssize loc;
};

#include "decl/slice-chop-decl.h"

// -----------------------------------------------------------------------------

static
struct vctrs_chop_indices* new_chop_indices(r_obj* x, r_obj* indices, r_obj* sizes) {
  r_obj* shelter = KEEP(r_alloc_list(4));

  r_obj* self = r_alloc_raw(sizeof(struct vctrs_chop_indices));
  r_list_poke(shelter, 0, self);

  struct vctrs_chop_indices* p_indices = r_raw_begin(self);
  p_indices->shelter = shelter;

  p_indices->indices = indices;
  r_list_poke(p_indices->shelter, 1, p_indices->indices);
  p_indices->has_indices = p_indices->indices != r_null;

  p_indices->sizes = sizes;
  r_list_poke(p_indices->shelter, 2, p_indices->sizes);
  p_indices->has_sizes = p_indices->sizes != r_null;

  if (p_indices->has_indices) {
    p_indices->v_indices = r_list_cbegin(p_indices->indices);
    p_indices->v_sizes = NULL;
    p_indices->index = r_null;
    r_list_poke(p_indices->shelter, 3, p_indices->index);
    p_indices->p_index = NULL;
  } else if (p_indices->has_sizes) {
    p_indices->v_indices = NULL;
    p_indices->v_sizes = r_int_cbegin(p_indices->sizes);
    p_indices->index = compact_seq(0, 0, true);
    r_list_poke(p_indices->shelter, 3, p_indices->index);
    p_indices->p_index = r_int_begin(p_indices->index);
  } else {
    p_indices->v_indices = NULL;
    p_indices->v_sizes = NULL;
    p_indices->index = r_int(0);
    r_list_poke(p_indices->shelter, 3, p_indices->index);
    p_indices->p_index = r_int_begin(p_indices->index);
  }

  p_indices->loc = 0;

  FREE(1);
  return p_indices;
}

/*
 * Generate the next `index`
 *
 * You can assume that the returned `index` is always protected by `p_indices`,
 * so the caller doesn't need to protect it.
 */
static inline
r_obj* indices_next(struct vctrs_chop_indices* p_indices) {
  const r_ssize loc = p_indices->loc;
  ++(p_indices->loc);

  if (p_indices->has_indices) {
    return p_indices->v_indices[loc];
  } else if (p_indices->has_sizes) {
    const r_ssize start = p_indices->p_index[0] + p_indices->p_index[1];
    const r_ssize size = p_indices->v_sizes[loc];
    const bool increasing = true;
    init_compact_seq(p_indices->p_index, start, size, increasing);
    return p_indices->index;
  } else {
    *p_indices->p_index = loc + 1;
    return p_indices->index;
  }
}

static inline
r_ssize indices_out_size(struct vctrs_chop_indices* p_indices, r_obj* x) {
  if (p_indices->has_indices) {
    return r_length(p_indices->indices);
  } else if (p_indices->has_sizes) {
    return r_length(p_indices->sizes);
  } else {
    return vec_size(x);
  }
}

// -----------------------------------------------------------------------------

r_obj* ffi_vec_chop_seq(r_obj* x, r_obj* starts, r_obj* sizes, r_obj* increasings) {
  int* v_starts = r_int_begin(starts);
  int* v_sizes = r_int_begin(sizes);
  int* v_increasings = r_lgl_begin(increasings);

  const r_ssize n = r_length(starts);

  r_obj* indices = KEEP(r_alloc_list(n));

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* index = compact_seq(v_starts[i], v_sizes[i], v_increasings[i]);
    r_list_poke(indices, i, index);
  }

  r_obj* out = KEEP(vec_chop_unsafe(x, indices, r_null));

  FREE(2);
  return out;
}

r_obj* ffi_vec_chop(r_obj* x, r_obj* indices, r_obj* sizes) {
  return vec_chop(x, indices, sizes);
}

r_obj* vec_chop(r_obj* x, r_obj* indices, r_obj* sizes) {
  const r_ssize n = vec_size(x);
  r_obj* names = KEEP(vec_names(x));

  if (indices != r_null && sizes != r_null) {
    r_abort_lazy_call(r_lazy_null, "Can't supply both `indices` and `sizes`.");
  }

  if (indices != r_null) {
    const bool allow_compact = false;
    indices = list_as_locations(indices, n, names, allow_compact);
  }
  KEEP(indices);

  if (sizes != r_null) {
    sizes = vec_as_chop_sizes(sizes, n);
  }
  KEEP(sizes);

  r_obj* out = vec_chop_unsafe(x, indices, sizes);

  FREE(3);
  return out;
}

// Performance variant that doesn't check the types or values of `indices` / `sizes`
r_obj* vec_chop_unsafe(r_obj* x, r_obj* indices, r_obj* sizes) {
  struct vctrs_proxy_info info = vec_proxy_info(x);
  KEEP(info.inner);

  struct vctrs_chop_indices* p_indices = new_chop_indices(x, indices, sizes);
  KEEP(p_indices->shelter);

  r_obj* out = vec_chop_base(x, info, p_indices);

  FREE(2);
  return out;
}

static
r_obj* vec_chop_base(r_obj* x,
                     struct vctrs_proxy_info info,
                     struct vctrs_chop_indices* p_indices) {
  if (vec_requires_fallback(x, info)) {
    // Fallback to `[` if the class doesn't implement a proxy. This is
    // to be maximally compatible with existing classes.
    if (info.type == VCTRS_TYPE_scalar) {
      r_abort_lazy_call(r_lazy_null, "Can't slice a scalar");
    }

    if (has_dim(x)) {
      return chop_fallback_shaped(x, p_indices);
    } else {
      return chop_fallback(x, p_indices);
    }
  }

  switch (info.type) {
  case VCTRS_TYPE_logical:
  case VCTRS_TYPE_integer:
  case VCTRS_TYPE_double:
  case VCTRS_TYPE_complex:
  case VCTRS_TYPE_character:
  case VCTRS_TYPE_raw:
  case VCTRS_TYPE_list: {
    if (has_dim(x)) {
      return chop_shaped(x, info, p_indices);
    } else {
      return chop(x, info, p_indices);
    }
  }
  case VCTRS_TYPE_dataframe: {
    return chop_df(x, info, p_indices);
  }
  default:
    obj_check_vector(x, vec_args.empty, r_lazy_null);
    stop_unimplemented_vctrs_type("vec_chop_base", info.type);
  }
}

static
r_obj* chop(r_obj* x,
            struct vctrs_proxy_info info,
            struct vctrs_chop_indices* p_indices) {
  r_obj* proxy = info.inner;
  r_obj* names = KEEP(r_names(proxy));
  const enum vctrs_type type = info.type;

  const r_ssize out_size = indices_out_size(p_indices, proxy);
  r_obj* out = KEEP(r_alloc_list(out_size));

  // Treat `elt` as owned after slicing (we also poke its names directly).
  // `vec_proxy_info()` doesn't recursively proxy.
  const struct vec_restore_opts elt_restore_opts = {
    .ownership = VCTRS_OWNERSHIP_shallow,
    .recursively_proxied = false
  };

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* index = indices_next(p_indices);

    // Always materialize ALTREP vectors when chopping to avoid inefficiently
    // creating a large amount of small ALTREP objects that are used downstream.
    // This is a heuristic and we should also be on the lookout for cases where
    // we chop to create a small amount of large ALTREP objects that are
    // quickly discarded (#1450).
    r_obj* elt = KEEP(vec_slice_base(
      type,
      proxy,
      index,
      VCTRS_MATERIALIZE_true
    ));

    if (names != r_null) {
      r_obj* elt_names = slice_names(names, index);
      r_attrib_poke_names(elt, elt_names);
    }

    elt = vec_restore_opts(elt, x, &elt_restore_opts);
    r_list_poke(out, i, elt);

    FREE(1);
  }

  FREE(2);
  return out;
}

static
r_obj* chop_df(r_obj* x,
               struct vctrs_proxy_info info,
               struct vctrs_chop_indices* p_indices) {
  r_obj* proxy = info.inner;
  r_obj* const* v_proxy = r_list_cbegin(proxy);

  const r_ssize n_cols = r_length(proxy);

  r_obj* col_names = KEEP(r_names(proxy));
  r_obj* row_names = KEEP(df_rownames(proxy));

  const bool has_row_names = r_typeof(row_names) == R_TYPE_character;

  const r_ssize out_size = indices_out_size(p_indices, proxy);
  r_obj* out = KEEP(r_alloc_list(out_size));
  r_obj* const* v_out = r_list_cbegin(out);

  // Pre-load the `out` container with empty bare data frames
  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* elt = r_alloc_list(n_cols);
    r_list_poke(out, i, elt);

    r_attrib_poke_names(elt, col_names);

    r_obj* index = indices_next(p_indices);
    const r_ssize size = vec_subscript_size(index);

    init_data_frame(elt, size);

    if (has_row_names) {
      r_obj* elt_row_names = slice_rownames(row_names, index);
      r_attrib_poke(elt, r_syms.row_names, elt_row_names);
    }
  }

  r_obj* indices = p_indices->indices;
  r_obj* sizes = p_indices->sizes;

  // Chop each column according to the indices, and then assign the results
  // into the appropriate data frame column in the `out` list
  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = v_proxy[i];
    r_obj* col_chopped = KEEP(vec_chop_unsafe(col, indices, sizes));
    r_obj* const* v_col_chopped = r_list_cbegin(col_chopped);

    for (r_ssize j = 0; j < out_size; ++j) {
      r_obj* elt = v_out[j];
      r_list_poke(elt, i, v_col_chopped[j]);
    }

    FREE(1);
  }

  // Each data frame container is owned by us.
  // Columns aren't necessarily owned by us, but that
  // doesn't matter because we don't recursively restore.
  // `vec_proxy_info()` doesn't recursively proxy.
  const struct vec_restore_opts elt_restore_opts = {
    .ownership = VCTRS_OWNERSHIP_shallow,
    .recursively_proxied = false
  };

  // Restore each data frame
  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* elt = v_out[i];
    elt = vec_restore_opts(elt, x, &elt_restore_opts);
    r_list_poke(out, i, elt);
  }

  FREE(3);
  return out;
}

static
r_obj* chop_shaped(r_obj* x,
                   struct vctrs_proxy_info info,
                   struct vctrs_chop_indices* p_indices) {
  r_obj* proxy = info.inner;
  const enum vctrs_type type = info.type;

  r_obj* dim_names = KEEP(r_dim_names(proxy));

  r_obj* row_names = r_null;
  if (dim_names != r_null) {
    row_names = r_list_get(dim_names, 0);
  }

  const r_ssize out_size = indices_out_size(p_indices, proxy);
  r_obj* out = KEEP(r_alloc_list(out_size));

  // Treat each `elt` as owned (we also poke its dim names)
  // `vec_proxy_info()` doesn't recursively proxy.
  const struct vec_restore_opts elt_restore_opts = {
    .ownership = VCTRS_OWNERSHIP_shallow,
    .recursively_proxied = false
  };

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* index = indices_next(p_indices);

    r_obj* elt = KEEP(vec_slice_shaped(type, proxy, index));

    if (dim_names != r_null) {
      if (row_names != r_null) {
        // Required to slice row names to the right size before poking to avoid
        // erroring on the dimnames length check in `Rf_setAttrib()`
        r_obj* new_dim_names = KEEP(r_clone(dim_names));
        r_obj* new_row_names = slice_names(row_names, index);
        r_list_poke(new_dim_names, 0, new_row_names);
        r_attrib_poke_dim_names(elt, new_dim_names);
        FREE(1);
      } else {
        r_attrib_poke_dim_names(elt, dim_names);
      }
    }

    elt = vec_restore_opts(elt, x, &elt_restore_opts);
    r_list_poke(out, i, elt);

    FREE(1);
  }

  FREE(2);
  return out;
}

static
r_obj* chop_fallback(r_obj* x, struct vctrs_chop_indices* p_indices) {
  // Evaluate in a child of the global environment to allow dispatch
  // to custom functions. We define `[` to point to its base
  // definition to ensure consistent look-up. This is the same logic
  // as in `vctrs_dispatch_n()`, reimplemented here to allow repeated
  // evaluations in a loop.
  r_obj* env = KEEP(r_alloc_empty_environment(r_envs.global));
  r_env_poke(env, syms_x, x);

  // Construct call with symbols, not values, for performance.
  // TODO - Remove once bit64 is updated on CRAN. Special casing integer64
  // objects to ensure correct slicing with `NA_integer_`.
  r_obj* call;
  if (is_integer64(x)) {
    call = KEEP(r_call3(syms.vec_slice_dispatch_integer64, syms_x, syms_i));
    r_env_poke(env, syms.vec_slice_dispatch_integer64, fns.vec_slice_dispatch_integer64);
  } else {
    call = KEEP(r_call3(syms_bracket, syms_x, syms_i));
    r_env_poke(env, syms_bracket, fns_bracket);
  }

  // Sliced `elt` comes from R, so is foreign. Technically not proxied at all,
  // so "restoring" is a bit of a hack, but we only restore if it looks like the
  // `[` result is missing attributes.
  struct vec_restore_opts elt_restore_opts = {
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };

  const r_ssize out_size = indices_out_size(p_indices, x);
  r_obj* out = KEEP(r_alloc_list(out_size));

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* index = indices_next(p_indices);

    index = KEEP(vec_subscript_materialize(index));

    // Update `i` binding with the new index value
    r_env_poke(env, syms_i, index);

    r_obj* elt = KEEP(r_eval(call, env));

    if (!vec_is_restored(elt, x)) {
      // No guarantee that we own `elt` here
      elt = vec_restore_opts(elt, x, &elt_restore_opts);
    }

    r_list_poke(out, i, elt);
    FREE(2);
  }

  FREE(3);
  return out;
}

static
r_obj* chop_fallback_shaped(r_obj* x, struct vctrs_chop_indices* p_indices) {
  const r_ssize out_size = indices_out_size(p_indices, x);
  r_obj* out = KEEP(r_alloc_list(out_size));

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* index = indices_next(p_indices);

    index = KEEP(vec_subscript_materialize(index));

    // `vec_slice_fallback()` will also `vec_restore()` for us
    r_obj* elt = vec_slice_fallback(x, index);
    r_list_poke(out, i, elt);

    FREE(1);
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

r_obj* list_as_locations(r_obj* indices, r_ssize n, r_obj* names, bool allow_compact) {
  if (r_typeof(indices) != R_TYPE_list) {
    r_abort_lazy_call(r_lazy_null, "`indices` must be a list of index values, or `NULL`.");
  }

  indices = KEEP(r_clone_referenced(indices));

  const r_ssize size = r_length(indices);
  r_obj* const* v_indices = r_list_cbegin(indices);

  // Restrict index values to positive integer locations
  // Also, notably, the `index` vector can't change size, i.e. `0` and `NA` aren't dropped.
  const struct location_opts opts = {
    .subscript_opts = {
      .logical = SUBSCRIPT_TYPE_ACTION_ERROR,
      .numeric = SUBSCRIPT_TYPE_ACTION_CAST,
      .character = SUBSCRIPT_TYPE_ACTION_ERROR
    },
    .missing = SUBSCRIPT_MISSING_PROPAGATE,
    .loc_negative = LOC_NEGATIVE_ERROR,
    .loc_oob = LOC_OOB_ERROR,
    .loc_zero = LOC_ZERO_ERROR
  };

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* index = v_indices[i];

    if (is_compact_seq(index)) {
      if (allow_compact) {
        // Allow `compact_seq` to pass through untouched,
        // assume caller can handle them natively
        continue;
      } else {
        // We don't want them to slip through when not handled natively
        r_stop_internal("`compact_seq` are not allowed.");
      }
    }

    index = vec_as_location_opts(index, n, names, &opts);
    r_list_poke(indices, i, index);
  }

  FREE(1);
  return indices;
}

static
r_obj* vec_as_chop_sizes(r_obj* sizes, r_ssize size) {
  sizes = KEEP(vec_cast(
    sizes,
    r_globals.empty_int,
    vec_args.sizes,
    vec_args.empty,
    r_lazy_null
  ));

  const r_ssize n_sizes = r_length(sizes);
  const int* v_sizes = r_int_cbegin(sizes);

  r_ssize total = 0;

  for (r_ssize i = 0; i < n_sizes; ++i) {
    const int elt = v_sizes[i];

    if (elt == r_globals.na_int) {
      r_abort_lazy_call(r_lazy_null, "`sizes` can't contain missing values.");
    } else if (elt < 0) {
      r_abort_lazy_call(r_lazy_null, "`sizes` can't contain negative sizes.");
    } else if (elt > size) {
      r_abort_lazy_call(r_lazy_null, "`sizes` can't contain sizes larger than %i.", size);
    }

    total += elt;
  }

  if (total != size) {
    r_abort_lazy_call(r_lazy_null, "`sizes` must sum to size %i, not size %i.", size, total);
  }

  FREE(1);
  return sizes;
}
