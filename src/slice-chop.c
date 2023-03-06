#include "vctrs.h"
#include "type-data-frame.h"

/*
 * @member shelter The shelter to protect the entire chop info.
 * @member proxy_info The result of `vec_proxy_info(x)`.
 * @member index The current index value. If `indices` are provided, this is
 *   the i-th element of indices. For the default of `indices = NULL`, this
 *   starts at 0 and is incremented by 1 repeatedly through `p_index`.
 * @member p_index A pointer to increment the `index` value for the default
 *   case.
 * @member has_indices Whether indices were provided.
 * @member out_size The size of `out`. Will be `vec_size(x)` in the default
 *   case, otherwise will be `vec_size(indices)`.
 * @member out The list container for the result.
 */
struct vctrs_chop_info {
  r_obj* shelter;

  struct vctrs_proxy_info proxy_info;

  r_obj* index;
  int* p_index;
  bool has_indices;

  r_ssize out_size;
  r_obj* out;
};

#include "decl/slice-chop-decl.h"

// -----------------------------------------------------------------------------

static
struct vctrs_chop_info new_chop_info(r_obj* x, r_obj* indices) {
  struct vctrs_chop_info info;
  info.shelter = KEEP(r_alloc_list(3));

  info.proxy_info = vec_proxy_info(x);
  r_list_poke(info.shelter, 0, info.proxy_info.shelter);

  info.index = r_int(0);
  r_list_poke(info.shelter, 1, info.index);
  info.p_index = r_int_begin(info.index);

  if (indices == r_null) {
    info.out_size = vec_size(x);
    info.has_indices = false;
  } else {
    info.out_size = vec_size(indices);
    info.has_indices = true;
  }

  info.out = r_alloc_list(info.out_size);
  r_list_poke(info.shelter, 2, info.out);

  FREE(1);
  return info;
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

  r_obj* out = KEEP(vec_chop(x, indices));

  FREE(2);
  return out;
}

r_obj* ffi_vec_chop(r_obj* x, r_obj* indices) {
  const r_ssize n = vec_size(x);
  r_obj* names = KEEP(vec_names(x));

  indices = KEEP(vec_as_indices(indices, n, names));

  r_obj* out = KEEP(vec_chop(x, indices));

  FREE(3);
  return out;
}

// [[ include("vctrs.h") ]]
r_obj* vec_chop(r_obj* x, r_obj* indices) {
  struct vctrs_chop_info info = new_chop_info(x, indices);
  KEEP(info.shelter);

  r_obj* out = vec_chop_base(x, indices, info);

  FREE(1);
  return out;
}

static r_obj* vec_chop_base(r_obj* x, r_obj* indices, struct vctrs_chop_info info) {
  struct vctrs_proxy_info proxy_info = info.proxy_info;

  // Fallback to `[` if the class doesn't implement a proxy. This is
  // to be maximally compatible with existing classes.
  if (vec_requires_fallback(x, proxy_info)) {
    if (proxy_info.type == VCTRS_TYPE_scalar) {
      r_abort_lazy_call(r_lazy_null, "Can't slice a scalar");
    }

    if (info.has_indices) {
      for (r_ssize i = 0; i < info.out_size; ++i) {
        r_obj* index = r_list_get(indices, i);

        if (is_compact(index)) {
          r_list_poke(indices, i, compact_materialize(index));
        }
      }
    }

    if (has_dim(x)) {
      return chop_fallback_shaped(x, indices, info);
    }

    return chop_fallback(x, indices, info);
  }

  switch (proxy_info.type) {
  case VCTRS_TYPE_logical:
  case VCTRS_TYPE_integer:
  case VCTRS_TYPE_double:
  case VCTRS_TYPE_complex:
  case VCTRS_TYPE_character:
  case VCTRS_TYPE_raw:
  case VCTRS_TYPE_list: {
    if (has_dim(x)) {
      return chop_shaped(x, indices, info);
    }

    return chop(x, indices, info);
  }
  case VCTRS_TYPE_dataframe: {
    return chop_df(x, indices, info);
  }
  default:
    obj_check_vector(x, vec_args.empty, r_lazy_null);
    stop_unimplemented_vctrs_type("vec_chop_base", proxy_info.type);
  }
}

static r_obj* chop(r_obj* x, r_obj* indices, struct vctrs_chop_info info) {
  r_obj* proxy = info.proxy_info.proxy;
  r_obj* names = KEEP(r_names(proxy));

  r_obj* const* v_indices = NULL;
  if (info.has_indices) {
    v_indices = r_list_cbegin(indices);
  }

  for (r_ssize i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = v_indices[i];
    } else {
      ++(*info.p_index);
    }

    // Always materialize ALTREP vectors when chopping to avoid inefficiently
    // creating a large amount of small ALTREP objects that are used downstream.
    // This is a heuristic and we should also be on the lookout for cases where
    // we chop to create a small amount of large ALTREP objects that are
    // quickly discarded (#1450).
    r_obj* elt = KEEP(vec_slice_base(
      info.proxy_info.type,
      proxy,
      info.index,
      VCTRS_MATERIALIZE_true
    ));

    if (names != r_null) {
      r_obj* elt_names = slice_names(names, info.index);
      r_attrib_poke_names(elt, elt_names);
    }

    elt = vec_restore(elt, x, vec_owned(elt));
    r_list_poke(info.out, i, elt);

    FREE(1);
  }

  FREE(1);
  return info.out;
}

static r_obj* chop_df(r_obj* x, r_obj* indices, struct vctrs_chop_info info) {
  r_obj* proxy = info.proxy_info.proxy;
  r_obj* const* v_proxy = r_list_cbegin(proxy);

  const r_ssize n_cols = r_length(proxy);

  r_obj* col_names = KEEP(r_names(proxy));
  r_obj* row_names = KEEP(df_rownames(proxy));

  bool has_row_names = r_typeof(row_names) == R_TYPE_character;

  r_obj* const* v_out = r_list_cbegin(info.out);

  r_obj* const* v_indices = NULL;
  if (info.has_indices) {
    v_indices = r_list_cbegin(indices);
  }

  // Pre-load the `out` container with empty bare data frames
  for (r_ssize i = 0; i < info.out_size; ++i) {
    r_obj* elt = r_alloc_list(n_cols);
    r_list_poke(info.out, i, elt);

    r_attrib_poke_names(elt, col_names);

    r_ssize size = -1;

    if (info.has_indices) {
      info.index = v_indices[i];
      size = vec_subscript_size(info.index);
    } else {
      ++(*info.p_index);
      size = 1;
    }

    init_data_frame(elt, size);

    if (has_row_names) {
      r_obj* elt_row_names = slice_rownames(row_names, info.index);
      r_attrib_poke(elt, R_RowNamesSymbol, elt_row_names);
    }
  }

  // Chop each column according to the indices, and then assign the results
  // into the appropriate data frame column in the `out` list
  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = v_proxy[i];
    r_obj* col_chopped = KEEP(vec_chop(col, indices));
    r_obj* const* v_col_chopped = r_list_cbegin(col_chopped);

    for (r_ssize j = 0; j < info.out_size; ++j) {
      r_obj* elt = v_out[j];
      r_list_poke(elt, i, v_col_chopped[j]);
    }

    FREE(1);
  }

  // Restore each data frame
  for (r_ssize i = 0; i < info.out_size; ++i) {
    r_obj* elt = v_out[i];
    elt = vec_restore(elt, x, vec_owned(elt));
    r_list_poke(info.out, i, elt);
  }

  FREE(2);
  return info.out;
}

static r_obj* chop_shaped(r_obj* x, r_obj* indices, struct vctrs_chop_info info) {
  r_obj* proxy = info.proxy_info.proxy;
  r_obj* dim_names = KEEP(r_dim_names(proxy));

  r_obj* row_names = r_null;
  if (dim_names != r_null) {
    row_names = r_list_get(dim_names, 0);
  }

  r_obj* const* v_indices = NULL;
  if (info.has_indices) {
    v_indices = r_list_cbegin(indices);
  }

  for (r_ssize i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = v_indices[i];
    } else {
      ++(*info.p_index);
    }

    r_obj* elt = KEEP(vec_slice_shaped(info.proxy_info.type, proxy, info.index));

    if (dim_names != r_null) {
      if (row_names != r_null) {
        // Required to slice row names to the right size before poking to avoid
        // erroring on the dimnames length check in `Rf_setAttrib()`
        r_obj* new_dim_names = KEEP(r_clone(dim_names));
        r_obj* new_row_names = slice_names(row_names, info.index);
        r_list_poke(new_dim_names, 0, new_row_names);
        r_attrib_poke_dim_names(elt, new_dim_names);
        FREE(1);
      } else {
        r_attrib_poke_dim_names(elt, dim_names);
      }
    }

    elt = vec_restore(elt, x, vec_owned(elt));
    r_list_poke(info.out, i, elt);

    FREE(1);
  }

  FREE(1);
  return info.out;
}

static r_obj* chop_fallback(r_obj* x, r_obj* indices, struct vctrs_chop_info info) {
  // Evaluate in a child of the global environment to allow dispatch
  // to custom functions. We define `[` to point to its base
  // definition to ensure consistent look-up. This is the same logic
  // as in `vctrs_dispatch_n()`, reimplemented here to allow repeated
  // evaluations in a loop.
  r_obj* env = KEEP(r_new_environment(r_envs.global));
  r_env_poke(env, syms_x, x);
  r_env_poke(env, syms_i, info.index);

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

  r_obj* const* v_indices = NULL;
  if (info.has_indices) {
    v_indices = r_list_cbegin(indices);
  }

  for (r_ssize i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = v_indices[i];
      // Update `i` binding with the new index value
      r_env_poke(env, syms_i, info.index);
    } else {
      ++(*info.p_index);
    }

    r_obj* elt = KEEP(r_eval(call, env));

    if (!vec_is_restored(elt, x)) {
      elt = vec_restore(elt, x, vec_owned(elt));
    }

    r_list_poke(info.out, i, elt);
    FREE(1);
  }

  FREE(2);
  return info.out;
}

static r_obj* chop_fallback_shaped(r_obj* x, r_obj* indices, struct vctrs_chop_info info) {
  r_obj* const* v_indices = NULL;
  if (info.has_indices) {
    v_indices = r_list_cbegin(indices);
  }

  for (r_ssize i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = v_indices[i];
    } else {
      ++(*info.p_index);
    }

    // `vec_slice_fallback()` will also `vec_restore()` for us
    r_obj* elt = vec_slice_fallback(x, info.index);
    r_list_poke(info.out, i, elt);
  }

  return info.out;
}

// -----------------------------------------------------------------------------

r_obj* vec_as_indices(r_obj* indices, r_ssize n, r_obj* names) {
  if (indices == r_null) {
    return indices;
  }

  if (r_typeof(indices) != R_TYPE_list) {
    r_abort_lazy_call(r_lazy_null, "`indices` must be a list of index values, or `NULL`.");
  }

  indices = KEEP(r_clone_referenced(indices));

  const r_ssize size = r_length(indices);
  r_obj* const* v_indices = r_list_cbegin(indices);

  // Restrict index values to positive integer locations
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
    index = vec_as_location_opts(index, n, names, &opts);
    r_list_poke(indices, i, index);
  }

  FREE(1);
  return indices;
}
