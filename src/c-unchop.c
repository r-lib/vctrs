#include "rlang-types.h"
#include "vctrs.h"

enum fallback_homogeneous {
  FALLBACK_HOMOGENEOUS_false = 0,
  FALLBACK_HOMOGENEOUS_true
};

#include "decl/c-unchop-decl.h"


static
r_obj* list_unchop(r_obj* xs,
                   r_obj* indices,
                   r_obj* ptype,
                   r_obj* name_spec,
                   const struct name_repair_opts* name_repair) {
  if (!vec_is_list(xs)) {
    r_abort("`x` must be a list.");
  }

  if (indices == r_null) {
    return vec_c(xs, ptype, name_spec, name_repair);
  }

  r_ssize xs_size = vec_size(xs);

  // Apply size/type checking to `indices` before possibly exiting early from
  // having a `NULL` common type
  if (xs_size != vec_size(indices)) {
    r_abort("`x` and `indices` must be lists of the same size.");
  }

  if (!vec_is_list(indices)) {
    r_abort("`indices` must be a list of integers, or `NULL`.");
  }

  ptype = KEEP(vec_ptype_common_params(xs,
                                       ptype,
                                       DF_FALLBACK_DEFAULT,
                                       S3_FALLBACK_true,
                                       vec_args.empty,
                                       r_lazy_null));

  if (needs_vec_c_fallback(ptype)) {
    r_obj* out = list_unchop_fallback(ptype, xs, indices, name_spec, name_repair, FALLBACK_HOMOGENEOUS_false);
    FREE(1);
    return out;
  }

  // FIXME: Needed for dplyr::summarise() which passes a non-fallback ptype
  if (needs_vec_c_homogeneous_fallback(xs, ptype)) {
    r_obj* out = list_unchop_fallback(ptype, xs, indices, name_spec, name_repair, FALLBACK_HOMOGENEOUS_true);
    FREE(1);
    return out;
  }

  if (ptype == r_null) {
    FREE(1);
    return r_null;
  }

  xs = KEEP(vec_cast_common(xs, ptype, vec_args.empty, r_lazy_null));

  bool assign_names = !r_inherits(name_spec, "rlang_zap");
  r_obj* xs_names = KEEP(r_names(xs));
  bool xs_is_named = xs_names != r_null && !is_data_frame(ptype);

  r_ssize out_size = 0;

  // `out_size` is computed from `indices`
  for (r_ssize i = 0; i < xs_size; ++i) {
    r_obj* x = r_list_get(xs, i);

    if (x == r_null) {
      continue;
    }

    r_ssize index_size = r_length(r_list_get(indices, i));
    out_size += index_size;

    // Each element of `xs` is recycled to its corresponding index's size
    x = vec_check_recycle(x, index_size, vec_args.empty, r_lazy_null);
    r_list_poke(xs, i, x);
  }

  r_obj* locs = KEEP(vec_as_indices(indices, out_size, r_null));

  r_obj* proxy = vec_proxy_recurse(ptype);
  r_keep_loc proxy_pi;
  KEEP_HERE(proxy, &proxy_pi);

  proxy = vec_init(proxy, out_size);
  KEEP_AT(proxy, proxy_pi);

  r_obj* out_names = r_null;
  r_keep_loc out_names_pi;
  KEEP_HERE(out_names, &out_names_pi);

  const struct vec_assign_opts unchop_assign_opts = {
    .recursive = true,
    .assign_names = assign_names,
    .ignore_outer_names = true
  };

  for (r_ssize i = 0; i < xs_size; ++i) {
    r_obj* x = r_list_get(xs, i);

    if (x == r_null) {
      continue;
    }

    r_obj* loc = r_list_get(locs, i);

    if (assign_names) {
      r_ssize size = r_length(loc);
      r_obj* outer = xs_is_named ? r_chr_get(xs_names, i) : r_null;
      r_obj* inner = KEEP(vec_names(x));
      r_obj* x_nms = KEEP(apply_name_spec(name_spec, outer, inner, size));

      if (x_nms != r_null) {
        R_LAZY_ALLOC(out_names, out_names_pi, R_TYPE_character, out_size);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (x_nms != chrs_empty) {
          out_names = chr_assign(out_names, loc, x_nms, VCTRS_OWNED_true);
          KEEP_AT(out_names, out_names_pi);
        }
      }

      FREE(2);
    }

    // Total ownership of `proxy` because it was freshly created with `vec_init()`
    proxy = vec_proxy_assign_opts(proxy, loc, x, VCTRS_OWNED_true, &unchop_assign_opts);
    KEEP_AT(proxy, proxy_pi);
  }

  r_obj* out = KEEP(vec_restore_recurse(proxy, ptype, VCTRS_OWNED_true));

  if (out_names != r_null) {
    out_names = KEEP(vec_as_names(out_names, name_repair));
    out = vec_set_names(out, out_names);
    FREE(1);
  } else if (!assign_names) {
    // FIXME: `vec_ptype2()` doesn't consistently zaps names, so `out`
    // might have been initialised with names. This branch can be
    // removed once #1020 is resolved.
    out = vec_set_names(out, r_null);
  }

  FREE(7);
  return out;
}

r_obj* ffi_list_unchop(r_obj* x,
                       r_obj* indices,
                       r_obj* ptype,
                       r_obj* name_spec,
                       r_obj* name_repair) {
  struct name_repair_opts name_repair_opts =
    new_name_repair_opts(name_repair,
                         r_lazy_null,
                         // think about this 'false'
                         false,
                         r_lazy_null);
  KEEP(name_repair_opts.shelter);

  r_obj* out = list_unchop(x, indices, ptype, name_spec, &name_repair_opts);

  FREE(1);
  return out;
}


// This is essentially:
// vec_slice_fallback(vec_c_fallback_invoke(!!!x), order(vec_c(!!!indices)))
// with recycling of each element of `x` to the corresponding index size
static
r_obj* list_unchop_fallback(r_obj* ptype,
                            r_obj* x,
                            r_obj* indices,
                            r_obj* name_spec,
                            const struct name_repair_opts* name_repair,
                            enum fallback_homogeneous homogeneous) {
  r_ssize x_size = vec_size(x);
  x = KEEP(r_clone_referenced(x));

  r_ssize out_size = 0;

  // Recycle `x` elements to the size of their corresponding index
  for (r_ssize i = 0; i < x_size; ++i) {
    r_obj* elt = r_list_get(x, i);

    r_ssize index_size = vec_size(r_list_get(indices, i));
    out_size += index_size;

    r_list_poke(x, i, vec_recycle_fallback(elt, index_size, vec_args.empty));
  }

  indices = KEEP(vec_as_indices(indices, out_size, r_null));

  r_obj* out = r_null;
  if (homogeneous) {
    out = KEEP(vec_c_fallback_invoke(x, name_spec));
  } else {
    out = KEEP(vec_c_fallback(ptype, x, name_spec, name_repair));
  }

  const struct name_repair_opts name_repair_opts = {
    .type = NAME_REPAIR_none,
    .fn = r_null
  };

  indices = KEEP(vec_c(
    indices,
    r_globals.empty_int,
    r_null,
    &name_repair_opts
  ));

  const int* p_indices = r_int_cbegin(indices);

  r_obj* locations = KEEP(r_alloc_integer(out_size));
  int* p_locations = r_int_begin(locations);

  // Initialize with missing to handle locations that are never selected
  for (r_ssize i = 0; i < out_size; ++i) {
    p_locations[i] = r_globals.na_int;
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    const int index = p_indices[i];

    if (index == r_globals.na_int) {
      continue;
    }

    p_locations[index - 1] = i + 1;
  }

  out = KEEP(vec_slice_fallback(out, locations));

  FREE(6);
  return out;
}
