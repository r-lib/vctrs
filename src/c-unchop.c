#include "rlang-types.h"
#include "vctrs.h"
#include "vec-bool.h"

enum fallback_homogeneous {
  FALLBACK_HOMOGENEOUS_false = 0,
  FALLBACK_HOMOGENEOUS_true
};

#include "decl/c-unchop-decl.h"


r_obj* list_unchop(r_obj* xs,
                   r_obj* indices,
                   r_obj* default_,
                   r_obj* ptype,
                   struct optional_r_ssize size,
                   enum list_unchop_unmatched unmatched,
                   r_obj* name_spec,
                   const struct name_repair_opts* name_repair,
                   struct vctrs_arg* p_error_arg,
                   struct vctrs_arg* p_default_arg,
                   struct r_lazy error_call) {
  obj_check_list(xs, p_error_arg, error_call);

  if (indices == r_null) {
    // Check if `default` and `size` were specified before reverting to `vec_c()`
    if (default_ != r_null) {
      r_abort("`indices` must be specified when `default` is specified.");
    }
    if (optional_r_ssize_is_some(size)) {
      r_abort("`indices` must be specified when `size` is specified.");
    }

    return vec_c(xs, ptype, name_spec, name_repair, p_error_arg, error_call);
  }

  if (default_ != r_null && optional_r_ssize_is_none(size)) {
    r_abort("`size` must be specified when `default` is specified.");
  }

  // Apply size/type checking to `indices` before possibly early exiting from
  // having a `NULL` common type or needing to apply a fallback
  obj_check_list(indices, vec_args.indices, error_call);

  if (vec_size(xs) != vec_size(indices)) {
    r_abort("`x` and `indices` must be lists of the same size.");
  }

  // `out_size` is computed from `indices` unless `size` is explicitly specified
  r_ssize out_size = 0;
  if (optional_r_ssize_is_some(size)) {
    out_size = optional_r_ssize_unwrap(size);
  } else {
    const r_ssize indices_size = vec_size(indices);
    for (r_ssize i = 0; i < indices_size; ++i) {
      out_size += r_length(r_list_get(indices, i));
    }
  }

  if (default_ != r_null) {
    // Validate `default` vector-ness and size. Type validation is later on.
    obj_check_vector(default_, p_default_arg, error_call);

    if (vec_size(default_) != 1) {
      vec_check_size(default_, out_size, p_default_arg, error_call);
    }
  }

  indices = KEEP(list_as_locations(indices, out_size, r_null));

  switch (unmatched) {
  case LIST_UNCHOP_UNMATCHED_default: {
    // Will use `default` in unmatched locations
    break;
  }
  case LIST_UNCHOP_UNMATCHED_error: {
    if (default_ != r_null) {
      r_abort("Can't set `default` when `unmatched = \"error\"`.");
    }
    check_any_unmatched(indices, out_size, error_call);
    break;
  }
  default: {
    r_stop_unreachable();
  }
  }

  ptype = KEEP(ptype_common_with_default(
    ptype,
    xs,
    default_,
    p_error_arg,
    p_default_arg,
    error_call
  ));

  if (needs_vec_c_fallback(ptype)) {
    r_obj* out = list_unchop_fallback(
      ptype,
      xs,
      indices,
      default_,
      out_size,
      unmatched,
      name_spec,
      name_repair,
      FALLBACK_HOMOGENEOUS_false,
      p_error_arg,
      p_default_arg,
      error_call
    );
    FREE(2);
    return out;
  }

  // FIXME: Needed for dplyr::summarise() which passes a non-fallback ptype
  if (needs_vec_c_homogeneous_fallback(xs, ptype)) {
    r_obj* out = list_unchop_fallback(
      ptype,
      xs,
      indices,
      default_,
      out_size,
      unmatched,
      name_spec,
      name_repair,
      FALLBACK_HOMOGENEOUS_true,
      p_error_arg,
      p_default_arg,
      error_call
    );
    FREE(2);
    return out;
  }

  if (ptype == r_null && optional_r_ssize_is_some(size)) {
    // User has requested an explicit output `size`. Even if there are no inputs
    // and we don't know the `ptype`, we should return something with this
    // `size`. Our size preserving identity type is `<unspecified[size]>`, so we
    // use that. Assuming `NULL` is roughly equivalent to `unspecified(0)`, this
    // gives us:
    //
    // ```
    // vec_c()
    // #> NULL
    // list_unchop(list(), indices = list())
    // #> NULL
    // list_unchop(list(), indices = list(), size = 0)
    // #> unspecified[0] # Roughly `NULL`, good enough.
    // list_unchop(list(), indices = list(), size = 5)
    // #> unspecified[5] # Preserves size, good.
    // ```
    //
    // The most theoretically correct thing may be to return `unspecified(0)`
    // from all cases here, but we make a concious effort to avoid exposing this
    // type to users when we can. This is a case of that compromise.
    // https://github.com/r-lib/vctrs/issues/1980
    ptype = vctrs_shared_empty_uns;
  }

  if (ptype == r_null) {
    // Unknown `ptype` due to having no user provided inputs and no explicit
    // `ptype` argument and no user requested `size`. In this case, since the
    // output size is always 0 and we want to be consistent with `vec_c()`,
    // we return `NULL`.
    FREE(2);
    return r_null;
  }

  // We've already done vector-ness and size checks on `default`, but we haven't
  // done type checks. Go ahead and early cast it to `ptype` before folding it
  // into `xs`. This gives us the best error messages (erroring early, while we
  // have `p_default_arg`) while still retaining a simple and consistent
  // implementation below.
  if (default_ != r_null) {
    r_obj* default_names = KEEP(vec_names(default_));
    default_ = KEEP(vec_cast(default_, ptype, p_default_arg, vec_args.empty, error_call));

    // FIXME: `vec_cast()` can currently drop names, so we carefully add them back
    // if it looks like casting dropped them. This should be removed eventually.
    // https://github.com/r-lib/vctrs/issues/623
    if (default_names != r_null && vec_names(default_) == r_null) {
      default_ = vec_set_names(default_, default_names);
    }
    KEEP(default_);

    FREE(3);
  }
  KEEP(default_);

  // Now merge `default` information into `xs` and `indices`
  if (default_ != r_null) {
    r_obj* default_index = KEEP(compute_default_index(indices, out_size));
    default_ = KEEP(slice_default(default_, default_index, out_size));

    xs = KEEP(push_default(xs, default_));
    indices = KEEP(push_default_index(indices, default_index));

    FREE(4);
  }
  KEEP(xs);
  KEEP(indices);

  const r_ssize xs_size = vec_size(xs);

  bool assign_names = !r_inherits(name_spec, "rlang_zap");
  r_obj* xs_names = KEEP(r_names(xs));
  bool xs_is_named = xs_names != r_null && !is_data_frame(ptype);

  r_obj* proxy = vec_proxy_recurse(ptype);
  r_keep_loc proxy_pi;
  KEEP_HERE(proxy, &proxy_pi);

  proxy = vec_init(proxy, out_size);
  KEEP_AT(proxy, proxy_pi);

  r_obj* out_names = r_null;
  r_keep_loc out_names_pi;
  KEEP_HERE(out_names, &out_names_pi);

  r_ssize i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    p_error_arg,
    xs_names,
    xs_size,
    &i
  );
  KEEP(p_x_arg->shelter);

  struct cast_opts unchop_cast_opts = {
    .to = ptype,
    .p_x_arg = p_x_arg,
    .call = error_call
  };

  const struct vec_assign_opts unchop_assign_opts = {
    .recursive = true,
    .assign_names = assign_names,
    .ignore_outer_names = true,
    .call = error_call
  };

  for (; i < xs_size; ++i) {
    r_obj* x = r_list_get(xs, i);

    if (x == r_null) {
      continue;
    }

    r_obj* index = r_list_get(indices, i);
    const r_ssize index_size = r_length(index);

    // Each element of `xs` is recycled to its corresponding index's size
    x = KEEP(vec_check_recycle(x, index_size, p_x_arg, error_call));

    if (assign_names) {
      r_obj* outer = xs_is_named ? r_chr_get(xs_names, i) : r_null;
      r_obj* inner = KEEP(vec_names(x));
      r_obj* x_nms = KEEP(apply_name_spec(name_spec, outer, inner, index_size));

      if (x_nms != r_null) {
        R_LAZY_ALLOC(out_names, out_names_pi, R_TYPE_character, out_size);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (x_nms != chrs_empty) {
          out_names = chr_assign(out_names, index, x_nms, VCTRS_OWNED_true);
          KEEP_AT(out_names, out_names_pi);
        }
      }

      FREE(2);
    }

    unchop_cast_opts.x = x;
    x = KEEP(vec_cast_opts(&unchop_cast_opts));

    // Total ownership of `proxy` because it was freshly created with `vec_init()`
    proxy = vec_proxy_assign_opts(proxy, index, x, VCTRS_OWNED_true, &unchop_assign_opts);
    KEEP_AT(proxy, proxy_pi);

    FREE(2);
  }

  if (is_data_frame(proxy)) {
    df_c_fallback(proxy, ptype, xs, out_size, name_spec, name_repair, error_call);
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

  FREE(10);
  return out;
}

r_obj* ffi_list_unchop(r_obj* ffi_x,
                       r_obj* ffi_indices,
                       r_obj* ffi_default,
                       r_obj* ffi_ptype,
                       r_obj* ffi_size,
                       r_obj* ffi_unmatched,
                       r_obj* ffi_name_spec,
                       r_obj* ffi_name_repair,
                       r_obj* ffi_frame) {
  struct r_lazy error_arg_lazy = { .x = r_syms.error_arg, .env = ffi_frame };
  struct vctrs_arg error_arg = new_lazy_arg(&error_arg_lazy);

  struct r_lazy default_arg_lazy = { .x = syms.default_arg, .env = ffi_frame };
  struct vctrs_arg default_arg = new_lazy_arg(&default_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  struct optional_r_ssize size = parse_size(ffi_size);
  enum list_unchop_unmatched unmatched = parse_unmatched(ffi_unmatched, error_call);

  struct name_repair_opts name_repair_opts =
    new_name_repair_opts(ffi_name_repair,
                         r_lazy_null,
                         false,
                         error_call);
  KEEP(name_repair_opts.shelter);

  r_obj* out = list_unchop(
    ffi_x,
    ffi_indices,
    ffi_default,
    ffi_ptype,
    size,
    unmatched,
    ffi_name_spec,
    &name_repair_opts,
    &error_arg,
    &default_arg,
    error_call
  );

  FREE(1);
  return out;
}


// This is essentially:
// vec_slice_fallback(vec_c_fallback_invoke(!!!x), order(vec_c(!!!indices)))
// with recycling of each element of `x` to the corresponding index size
static
r_obj* list_unchop_fallback(
  r_obj* ptype,
  r_obj* xs,
  r_obj* indices,
  r_obj* default_,
  r_ssize out_size,
  enum list_unchop_unmatched unmatched,
  r_obj* name_spec,
  const struct name_repair_opts* name_repair,
  enum fallback_homogeneous homogenous,
  struct vctrs_arg* p_error_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  r_ssize xs_size = vec_size(xs);
  r_obj* xs_names = r_names(xs);
  xs = KEEP(r_clone_referenced(xs));

  r_ssize i = 0;

  struct vctrs_arg* p_x_arg = new_subscript_arg(
    p_error_arg,
    xs_names,
    xs_size,
    &i
  );
  KEEP(p_x_arg->shelter);

  // Recycle `xs` elements to the size of their corresponding location vector
  for (; i < xs_size; ++i) {
    r_obj* x = r_list_get(xs, i);
    r_ssize index_size = r_length(r_list_get(indices, i));
    r_list_poke(xs, i, vec_recycle_fallback(x, index_size, p_x_arg, error_call));
  }

  if (default_ != r_null) {
    r_obj* default_index = KEEP(compute_default_index(indices, out_size));
    default_ = KEEP(slice_default(default_, default_index, out_size));

    // Delay `push_default()` until we do optional ptype checks in the fallback method first
    indices = KEEP(push_default_index(indices, default_index));

    FREE(3);
  }
  KEEP(default_);
  KEEP(indices);

  r_obj* out = r_null;
  if (homogenous) {
    out = KEEP(list_unchop_c_fallback_invoke(xs, default_, name_spec, error_call));
  } else {
    out = KEEP(list_unchop_c_fallback(ptype, xs, default_, name_spec, name_repair, p_error_arg, p_default_arg, error_call));
  }

  const struct name_repair_opts name_repair_opts = {
    .type = NAME_REPAIR_none,
    .fn = r_null,
    .call = error_call
  };

  indices = KEEP(vec_c(
    indices,
    r_globals.empty_int,
    r_null,
    &name_repair_opts,
    vec_args.indices,
    error_call
  ));

  const r_ssize indices_size = r_length(indices);
  const int* p_indices = r_int_cbegin(indices);

  r_obj* locations = KEEP(r_alloc_integer(out_size));
  int* p_locations = r_int_begin(locations);

  // Initialize with missing to handle locations that are never selected
  for (r_ssize i = 0; i < out_size; ++i) {
    p_locations[i] = r_globals.na_int;
  }

  for (r_ssize i = 0; i < indices_size; ++i) {
    const int index = p_indices[i];

    if (index == r_globals.na_int) {
      continue;
    }

    p_locations[index - 1] = i + 1;
  }

  out = KEEP(vec_slice_fallback(out, locations));

  FREE(8);
  return out;
}

static
r_obj* list_unchop_c_fallback_invoke(
  r_obj* xs,
  r_obj* default_,
  r_obj* name_spec,
  struct r_lazy error_call
) {
  if (default_ != r_null) {
    xs = push_default(xs, default_);
  }
  KEEP(xs);

  r_obj* out = vec_c_fallback_invoke(xs, name_spec, error_call);

  FREE(1);
  return out;
}

// This is `vec_c_fallback()` with support for `default`.
// Keep aligned with `vec_c_fallback()`!
// TODO: Let `list_unchop()` own fallback helpers and
// make `vec_c_fallback()` and friends thin wrappers
// that set `default` to `r_null`, or merge `vec_c()`
// and `list_unchop()` entirely!
static
r_obj* list_unchop_c_fallback(
  r_obj* ptype,
  r_obj* xs,
  r_obj* default_,
  r_obj* name_spec,
  const struct name_repair_opts* name_repair,
  struct vctrs_arg* p_error_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  r_obj* cls = KEEP(r_attrib_get(ptype, syms_fallback_class));
  bool implements_c = class_implements_base_c(cls);

  if (implements_c) {
    FREE(1);
    return list_unchop_c_fallback_invoke(xs, default_, name_spec, error_call);
  }

  struct fallback_opts fallback_opts = {
    .s3 = S3_FALLBACK_false
  };

  struct ptype_common_opts ptype_opts = {
    .p_arg = p_error_arg,
    .call = error_call,
    .fallback = fallback_opts
  };

  // Should cause a common type error, unless another fallback
  // kicks in (for instance, homogeneous class with homogeneous
  // attributes)
  ptype = KEEP(vec_ptype_common_opts(xs, r_null, &ptype_opts));

  if (default_ != r_null) {
    // Also check against `default` using `p_default_arg`
    const struct ptype2_opts ptype2_opts = {
      .x = ptype,
      .y = default_,
      .p_x_arg = vec_args.empty,
      .p_y_arg = p_default_arg,
      .call = error_call,
      .fallback = fallback_opts
    };
    int _;
    vec_ptype2_opts(&ptype2_opts, &_);

    // Now finally `push_default()`
    xs = KEEP(push_default(xs, default_));

    FREE(1);
  }
  KEEP(xs);

  // Suboptimal: Call `vec_c()` again to combine vector with
  // homogeneous class fallback
  r_obj* out = vec_c_opts(
    xs,
    r_null,
    name_spec,
    name_repair,
    &ptype_opts.fallback,
    p_error_arg,
    error_call
  );

  FREE(3);
  return out;
}

static
void check_any_unmatched(
  r_obj* indices,
  r_ssize out_size,
  struct r_lazy call
) {
  struct r_vector_bool* p_unmatched = detect_unmatched(indices, out_size);
  KEEP(p_unmatched->shelter);

  if (r_vector_bool_any(p_unmatched)) {
    r_obj* loc = KEEP(r_vector_bool_which(p_unmatched));
    stop_unchop_unmatched(loc, call);
  }

  FREE(1);
}

static
void stop_unchop_unmatched(r_obj* loc, struct r_lazy call) {
  r_obj* syms[3] = {
    syms_loc,
    syms_call,
    NULL
  };
  r_obj* args[3] = {
    loc,
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_unchop_unmatched, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_unchop_unmatched");
}

static
r_obj* compute_default_index(
  r_obj* indices,
  r_ssize out_size
) {
  struct r_vector_bool* p_unmatched = detect_unmatched(indices, out_size);
  KEEP(p_unmatched->shelter);

  r_obj* out = r_vector_bool_which(p_unmatched);

  FREE(1);
  return out;
}

static
struct r_vector_bool* detect_unmatched(
  r_obj* indices,
  r_ssize out_size
) {
  const r_ssize indices_size = r_length(indices);
  r_obj* const* v_indices = r_list_cbegin(indices);

  struct r_vector_bool* p_unmatched = r_new_vector_bool(out_size);
  KEEP(p_unmatched->shelter);
  r_vector_bool_fill(p_unmatched, true);
  bool* v_unmatched = r_vector_bool_begin(p_unmatched);

  // Mark `unmatched` locations in the boolean vector
  for (r_ssize i = 0; i < indices_size; ++i) {
    r_obj* index = v_indices[i];

    const r_ssize index_size = r_length(index);
    const int* v_index = r_int_cbegin(index);

    for (r_ssize j = 0; j < index_size; ++j) {
      const int loc = v_index[j];

      if (loc == r_globals.na_int) {
        continue;
      }

      v_unmatched[loc - 1] = false;
    }
  }

  FREE(1);
  return p_unmatched;
}

static
r_obj* slice_default(
  r_obj* default_,
  r_obj* default_index,
  r_ssize out_size
) {
  const r_ssize default_size = vec_size(default_);

  if (default_size == 1) {
    // Recycle "up" to size of the index
    r_ssize default_index_size = vec_size(default_index);
    return vec_recycle(default_, default_index_size);
  }

  if (default_size == out_size) {
    // Slice "down" to the used elements
    return vec_slice_unsafe(default_, default_index);
  }

  r_stop_internal("Unexpected `default` size");
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

// `ptype` determination is complicated by the fact that both `xs` and `default`
// will contribute to the output type, and we want the best error messages
// possible. We can't just fold `default` into `xs` because we don't get
// a chance to use `p_default_arg`. We could materialize `p_default_arg`
// and use it as a name on the `xs` list, but that means it will get combined
// with `p_error_arg` when there is an error, which we don't want.
static
r_obj* ptype_common_with_default(
  r_obj* ptype,
  r_obj* xs,
  r_obj* default_,
  struct vctrs_arg* p_error_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy call
) {
  if (ptype != r_null) {
    // Performs scalar checks and whatnot
    return vec_ptype(ptype, vec_args.ptype, call);
  }

  // Okay `ptype` is `NULL`. We determine it from `xs` and `default`.

  const struct fallback_opts fallback_opts = {
    .s3 = S3_FALLBACK_true
  };

  // Use only `xs` and `p_error_arg` first for best errors
  ptype = KEEP(vec_ptype_common_params(xs, r_null, fallback_opts.s3, p_error_arg, call));

  // Now incorporate `default` and `p_default_arg`
  const struct ptype2_opts ptype2_opts = {
    .x = ptype,
    .y = default_,
    .p_x_arg = vec_args.empty,
    .p_y_arg = p_default_arg,
    .call = call,
    .fallback = fallback_opts
  };
  int _;
  ptype = vec_ptype2_opts(&ptype2_opts, &_);

  FREE(1);
  return ptype;
}

enum list_unchop_unmatched parse_unmatched(r_obj* unmatched, struct r_lazy call) {
  if (!r_is_string(unmatched)) {
    r_stop_internal("`unmatched` must be a string.");
  }

  const char* c_unmatched = r_chr_get_c_string(unmatched, 0);

  if (!strcmp(c_unmatched, "default")) return LIST_UNCHOP_UNMATCHED_default;
  if (!strcmp(c_unmatched, "error")) return LIST_UNCHOP_UNMATCHED_error;

  r_abort_lazy_call(
    call,
    "`unmatched` must be either \"default\" or \"error\"."
  );
}

struct optional_r_ssize parse_size(r_obj* size) {
  if (size == r_null) {
    return optional_r_ssize_none;
  } else {
    return new_optional_r_ssize(r_arg_as_ssize(size, "size"));
  }
}
