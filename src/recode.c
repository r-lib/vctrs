#include "recode.h"
#include "vctrs.h"
#include "vec-int.h"

#include "decl/recode-decl.h"

r_obj* vec_recode_values(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  bool from_as_list_of_vectors,
  bool to_as_list_of_vectors,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_from_arg,
  struct vctrs_arg* p_to_arg,
  struct vctrs_arg* p_default_arg,
  r_obj* ptype,
  struct r_lazy error_call
) {
  int n_prot = 0;

  const bool has_default = default_ != r_null;

  // Vector checks ----

  obj_check_vector(x, VCTRS_ALLOW_NULL_no, p_x_arg, error_call);

  if (from_as_list_of_vectors) {
    obj_check_list(from, p_from_arg, error_call);
    list_check_all_vectors(from, VCTRS_ALLOW_NULL_no, p_from_arg, error_call);
  } else {
    obj_check_vector(from, VCTRS_ALLOW_NULL_no, p_from_arg, error_call);
  }

  if (to_as_list_of_vectors) {
    obj_check_list(to, p_to_arg, error_call);
    list_check_all_vectors(to, VCTRS_ALLOW_NULL_no, p_to_arg, error_call);
  } else {
    obj_check_vector(to, VCTRS_ALLOW_NULL_no, p_to_arg, error_call);
  }

  if (has_default) {
    obj_check_vector(default_, VCTRS_ALLOW_NULL_no, p_default_arg, error_call);
  }

  // Type checks ----

  // Finalize `ptype` up front
  // Remember that `default` gets a chance to participate in `ptype` determination
  ptype = KEEP_N(
    ptype_finalize(
      ptype,
      to,
      default_,
      to_as_list_of_vectors,
      p_to_arg,
      p_default_arg,
      error_call
    ),
    &n_prot
  );

  // Cast `to` to the determined common type
  // Errors can happen here if user supplied `ptype`, short circuiting common type checks
  if (to_as_list_of_vectors) {
    to = KEEP_N(vec_cast_common(to, ptype, p_to_arg, error_call), &n_prot);
  } else {
    to = KEEP_N(vec_cast(to, ptype, p_to_arg, vec_args.empty, error_call), &n_prot);
  }

  // Cast `default` to determined common type
  if (has_default) {
    default_ = KEEP_N(
      vec_cast(
        default_,
        ptype,
        p_default_arg,
        vec_args.empty,
        error_call
      ),
      &n_prot
    );
  }

  // Cast `from` to the type of `x`
  if (from_as_list_of_vectors) {
    // TODO: `vec_cast_common()` should take `to_arg` so we can use `p_x_arg` here
    from = KEEP_N(vec_cast_common(from, x, p_from_arg, error_call), &n_prot);
  } else {
    from = KEEP_N(vec_cast(from, x, p_from_arg, p_x_arg, error_call), &n_prot);
  }

  // Size checks ----

  const r_ssize x_size = vec_size(x);
  r_ssize from_size = vec_size(from);
  const r_ssize to_size = vec_size(to);

  // Check size of `to` as recyclable against the size of `from`,
  // regardless of `from_as_list_of_vectors` and `to_as_list_of_vectors`
  vec_check_recyclable(to, from_size, VCTRS_ALLOW_NULL_no, p_to_arg, error_call);

  if (to_as_list_of_vectors) {
    // Each element of `to` should be recyclable against `x`.
    list_check_all_recyclable(to, x_size, VCTRS_ALLOW_NULL_no, p_to_arg, error_call);
  }

  if (has_default) {
    // `default` should be recycled against `x`.
    vec_check_recyclable(default_, x_size, VCTRS_ALLOW_NULL_no, p_default_arg, error_call);
  }

  // Implementation ----

  // At this point we don't expect any errors, all vector, type, and size
  // checks have been done. So `p_*_arg` and `error_call` are not used.

  // Try `to_as_list_of_vectors` optimization
  //
  // All size 1 `to` values is a common `dplyr::recode_values()` case
  if (to_as_list_of_vectors) {
    if (list_all_size(to, 1, VCTRS_ALLOW_NULL_no, p_to_arg, error_call)) {
      // Optimize `to` to a flat vector and drop any outer names on the list, we've
      // already done casting checks and we don't want them on the flat form. We
      // don't expect any errors here.
      to_as_list_of_vectors = false;

      to = KEEP_N(
        vec_c(
          to,
          ptype,
          name_spec_inner,
          p_no_repair_opts,
          vec_args.empty,
          r_lazy_null
        ),
        &n_prot
      );
    }
  }

  // Flatten `from` to a vector if it is a list
  r_obj* from_flat;

  if (from_as_list_of_vectors) {
    from_flat = KEEP_N(
      vec_c(
        from,
        x,
        name_spec_inner,
        p_no_repair_opts,
        vec_args.empty,
        r_lazy_null
      ),
      &n_prot
    );
  } else {
    from_flat = from;
  }

  // Compute `xs` and `indices` for `list_combine()`
  r_obj* xs;
  r_obj* indices;

  if (to_as_list_of_vectors) {
    if (to_size == 1) {
      xs = to;
      indices = KEEP_N(build_indices_for_single_to(x, from_flat), &n_prot);
    } else {
      xs = to;

      r_obj* from_flat_map = r_null;
      if (from_as_list_of_vectors && from_size != vec_size(from_flat)) {
        // `from` was flattened and that changed its size, we need a "map" back
        // into the unflattened form to place indices correctly
        from_flat_map = KEEP_N(build_from_flat_map(from, from_size), &n_prot);
      }

      indices = KEEP_N(
        build_indices_for_to_as_list_of_vectors(
          x,
          from_flat,
          from_flat_map,
          x_size,
          from_size
        ),
        &n_prot
      );
    }
  } else {
    if (to_size == 1) {
      xs = KEEP_N(r_list(to), &n_prot);
      indices = KEEP_N(build_indices_for_single_to(x, from_flat), &n_prot);
    } else {
      if (from_as_list_of_vectors && from_size != vec_size(from_flat)) {
        // `from` was flattened and that changed its size, we need to
        // repeat `to` elements to match the new flattened size
        to = KEEP_N(build_repeated_to(to, from), &n_prot);
      }

      r_obj* result = KEEP_N(
        build_xs_and_indices_for_to_as_vector(
          x,
          from_flat,
          to,
          x_size
        ),
        &n_prot
      );

      xs = r_list_get(result, 0);
      indices = r_list_get(result, 1);
    }
  }

  // The `indices` are actually built in such a way that `"first"` is
  // the effective behavior here, so it doesn't actually matter what
  // we provide. Regardless, we want `case_when()` like behavior.
  const enum list_combine_multiple multiple = LIST_COMBINE_MULTIPLE_last;

  // - With `to_as_list_of_vectors`, each `to` element was size 1 or `x_size`
  //   so can use `ASSIGNMENT_SLICE_VALUE_yes`.
  // - With `!to_as_list_of_vectors`, `to` was size 1 or `from_size` but has
  //   already been resliced to the `indices` size.
  enum assignment_slice_value slice_xs = to_as_list_of_vectors ?
    ASSIGNMENT_SLICE_VALUE_yes :
    ASSIGNMENT_SLICE_VALUE_no;

  r_obj* out = list_combine(
    xs,
    indices,
    x_size,
    default_,
    unmatched,
    multiple,
    slice_xs,
    ptype,
    name_spec_inner,
    p_no_repair_opts,
    vec_args.empty,
    vec_args.empty,
    vec_args.empty,
    error_call
  );

  FREE(n_prot);
  return out;
}

r_obj* vec_replace_values(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  bool from_as_list_of_vectors,
  bool to_as_list_of_vectors,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_from_arg,
  struct vctrs_arg* p_to_arg,
  struct r_lazy error_call
) {
  obj_check_vector(x, VCTRS_ALLOW_NULL_no, p_x_arg, error_call);

  r_obj* default_ = x;
  struct vctrs_arg* p_default_arg = p_x_arg;

  const enum list_combine_unmatched unmatched = LIST_COMBINE_UNMATCHED_default;
  r_obj* ptype = KEEP(vec_ptype_final(x, p_x_arg, error_call));

  r_obj* out = KEEP(vec_recode_values(
    x,
    from,
    to,
    default_,
    unmatched,
    from_as_list_of_vectors,
    to_as_list_of_vectors,
    p_x_arg,
    p_from_arg,
    p_to_arg,
    p_default_arg,
    ptype,
    error_call
  ));

  // `vec_recode_values()` creates a new vector and names come from any of `to`
  // or `default`, but `vec_replace_values()` updates an existing vector and
  // should act like `[<-` and `base::replace()`, retaining existing names.
  // `out` is totally fresh, so we can claim deep ownership over it (though we
  // only require shallow ownership to set names).
  r_obj* names = KEEP(vec_names(x));
  out = vec_set_names(out, names, VCTRS_OWNERSHIP_deep);

  FREE(3);
  return out;
}

r_obj* ffi_vec_recode_values(
  r_obj* ffi_x,
  r_obj* ffi_from,
  r_obj* ffi_to,
  r_obj* ffi_default,
  r_obj* ffi_unmatched,
  r_obj* ffi_from_as_list_of_vectors,
  r_obj* ffi_to_as_list_of_vectors,
  r_obj* ffi_ptype,
  r_obj* ffi_frame
) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy from_arg_lazy = { .x = syms.from_arg, .env = ffi_frame };
  struct vctrs_arg from_arg = new_lazy_arg(&from_arg_lazy);

  struct r_lazy to_arg_lazy = { .x = syms.to_arg, .env = ffi_frame };
  struct vctrs_arg to_arg = new_lazy_arg(&to_arg_lazy);

  struct r_lazy default_arg_lazy = { .x = syms.default_arg, .env = ffi_frame };
  struct vctrs_arg default_arg = new_lazy_arg(&default_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const bool from_as_list_of_vectors = r_arg_as_bool(ffi_from_as_list_of_vectors, "from_as_list_of_vectors");
  const bool to_as_list_of_vectors = r_arg_as_bool(ffi_to_as_list_of_vectors, "to_as_list_of_vectors");

  const enum list_combine_unmatched unmatched = parse_list_combine_unmatched(ffi_unmatched, error_call);

  return vec_recode_values(
    ffi_x,
    ffi_from,
    ffi_to,
    ffi_default,
    unmatched,
    from_as_list_of_vectors,
    to_as_list_of_vectors,
    &x_arg,
    &from_arg,
    &to_arg,
    &default_arg,
    ffi_ptype,
    error_call
  );
}

r_obj* ffi_vec_replace_values(
  r_obj* ffi_x,
  r_obj* ffi_from,
  r_obj* ffi_to,
  r_obj* ffi_from_as_list_of_vectors,
  r_obj* ffi_to_as_list_of_vectors,
  r_obj* ffi_frame
) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy from_arg_lazy = { .x = syms.from_arg, .env = ffi_frame };
  struct vctrs_arg from_arg = new_lazy_arg(&from_arg_lazy);

  struct r_lazy to_arg_lazy = { .x = syms.to_arg, .env = ffi_frame };
  struct vctrs_arg to_arg = new_lazy_arg(&to_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const bool from_as_list_of_vectors = r_arg_as_bool(ffi_from_as_list_of_vectors, "from_as_list_of_vectors");
  const bool to_as_list_of_vectors = r_arg_as_bool(ffi_to_as_list_of_vectors, "to_as_list_of_vectors");

  return vec_replace_values(
    ffi_x,
    ffi_from,
    ffi_to,
    from_as_list_of_vectors,
    to_as_list_of_vectors,
    &x_arg,
    &from_arg,
    &to_arg,
    error_call
  );
}

// Form `indices` when `to` is a list of vectors
//
// # Example
//
// ```
// x = c(1, 2, 3, 4, 5, 1)
//
// from = list(
//   c(1, 3),
//   c(4, 2, 1)
// )
//
// # Expected output
// indices = list(
//   c(1, 3, 6),
//   c(2, 4)
// )
// ```
//
// To get to our expected output, we flatten `from` into `from_flat` but track
// its original sizes through a `from_flat_map`
//
// ```
// from_flat     = c(1, 3, 4, 2, 1)
// from_flat_map = c(0, 0, 1, 1, 1)
// ```
//
// Then we match `x` against `from_flat` to only perform a single match call,
// and to ensure that the "first" match wins.
//
// ```
// loc = c(1, 4, 2, 3, NA, 1)
// ```
//
// We loop over `loc` to determine the size of each `index` that goes in the output.
//
// ```
// loc = c(1, 4, 2, 3, NA, 1)
//         _     _         _ These go with index 1, size 3
//            _     _        These go with index 2, size 2
// ```
//
// We determine that, say, `loc` 4 (1-indexed) from above goes with index 2 by looking at
// `from_flat_map[4 - 1]` and seeing that that gives us 1 (0-indexed) meaning it goes
// with index 2.
//
// We then construct the `indices` of the right size using this first pass, then
// do a second pass that finally accumulates the `indices` in the right buckets,
// using the same `from_flat_map` explanation as above.
//
// # Safety
//
// Expect that all type and size checks have been done.
static
r_obj* build_indices_for_to_as_list_of_vectors(
  r_obj* x,
  r_obj* from_flat,
  r_obj* from_flat_map,
  r_ssize x_size,
  r_ssize from_size
) {
  r_obj* indices = KEEP(r_alloc_list(from_size));

  const bool has_from_flat_map = from_flat_map != r_null;
  const r_ssize* v_from_flat_map = has_from_flat_map ? r_raw_cbegin(from_flat_map) : NULL;

  // Want `na_equal = true` for `vec_recode_values(x, from = NA, to = to)`
  const bool na_equal = true;

  // Find `x` values in `from_flat`
  r_obj* loc = KEEP(vec_match_params(
    x,
    from_flat,
    na_equal,
    vec_args.empty,
    vec_args.empty,
    r_lazy_null
  ));
  const int* v_loc = r_int_cbegin(loc);

  r_obj* index_sizes = KEEP(r_alloc_raw(from_size * sizeof(r_ssize)));
  r_ssize* v_index_sizes = r_raw_begin(index_sizes);
  r_memset(v_index_sizes, 0, from_size * sizeof(r_ssize));

  // First pass sums the `index` sizes. Using growables has too much
  // ambiguity when there are a large number of `from` elements.
  for (r_ssize i = 0; i < x_size; ++i) {
    int loc = v_loc[i];

    if (loc != r_globals.na_int) {
      loc = has_from_flat_map ? v_from_flat_map[loc - 1] : loc - 1;
      ++v_index_sizes[loc];
    }
  }

  // Direct pointer to the integer vectors we store in `indices`
  r_obj* v_indices = KEEP(r_alloc_raw(from_size * sizeof(int*)));
  int** v_v_indices = r_raw_begin(v_indices);

  // Allocate each `index`
  for (r_ssize i = 0; i < from_size; ++i) {
    const r_ssize index_size = v_index_sizes[i];
    r_obj* index = r_alloc_integer(index_size);
    r_list_poke(indices, i, index);
    v_v_indices[i] = r_int_begin(index);
  }

  // Set `v_index_sizes` to `0`. We are going to reuse this
  // to now track the location we are inserting at for a particular index.
  r_memset(v_index_sizes, 0, from_size * sizeof(r_ssize));
  r_ssize* v_index_locs = v_index_sizes;
  v_index_sizes = NULL;

  // Second pass inserts the `index` values
  for (r_ssize i = 0; i < x_size; ++i) {
    int loc = v_loc[i];

    if (loc != r_globals.na_int) {
      loc = has_from_flat_map ? v_from_flat_map[loc - 1] : loc - 1;
      int* v_index = v_v_indices[loc];
      const r_ssize index_loc = v_index_locs[loc];
      ++v_index_locs[loc];
      v_index[index_loc] = i + 1;
    }
  }

  FREE(4);
  return indices;
}

// Form `xs` and `indices` when `to` is a vector
//
// # Example
//
// Imagine you have:
//
// ```
// vec_recode_values(
//   x = c(1, 2, 3, 2),
//   from = c(3, 2),
//   to = c(-1, -2)
// )
// #> [1] NA -2 -1 -2
// ```
//
// To get this output, you `vec_match(x, from)` to get:
//
// ```
// vec_match(x, from)
// #> [1] NA 2 1 2
// ```
//
// Then compute `index_into_to` and `index_into_out` as:
//
// ```
// index_into_to = c(2, 1, 2)
// index_into_out = c(2, 3, 4)
//
// vec_slice(to, index_into_to)
// #> [1] -2 -1 -2
// ```
//
// Now you can take `list(to)` and `list(index_into_out)` as `xs` and `indices` into
// `list_combine()`. In theory the output could just be:
//
// ```
// vec_slice(to, vec_match(x, from))
// ```
//
// but this is simplifying over a few things:
//
// - No `default` support
// - No `unmatched` support
// - We always make a "fresh" output container in `list_combine()` with
//   `vec_init()`, which has implications for clearing extraneous that might
//   have been on `to`. Ideally we do drop any extraneous attributes not
//   relevant for the class before returning the attribute.
//
// So in the end it seems simpler to just go through `list_combine()` for
// all cases.
//
// # Safety
//
// Expect that all type and size checks have been done.
static
r_obj* build_xs_and_indices_for_to_as_vector(
  r_obj* x,
  r_obj* from_flat,
  r_obj* to,
  r_ssize x_size
) {
  r_obj* out = KEEP(r_alloc_list(2));

  const bool na_equal = true;

  // Find locations where `to` will be utilized
  // Want `na_equal = true` for `vec_recode_values(x, from = NA, to = to)`
  r_obj* loc = KEEP(vec_match_params(
    x,
    from_flat,
    na_equal,
    vec_args.empty,
    vec_args.empty,
    r_lazy_null
  ));

  // TODO: Might be nice if `vec_match()` optionally reported the number of matches
  // as an `n` attribute like `vec_group_id()`, not sure how that interacts with
  // `na_equal = false` though, do propagated `NA` count as matches?
  const r_ssize size = r_int_count_complete(loc);

  r_obj* xs = r_alloc_list(1);
  r_list_poke(out, 0, xs);

  r_obj* indices = r_alloc_list(1);
  r_list_poke(out, 1, indices);

  // `to_size != 1` otherwise we would have used
  // `build_indices_for_single_to()`, so `to` instead has size of `from_size`
  // and must be resliced and placed in the correct order.
  if (size == x_size) {
    // It's fairly common to match every value in `x` when recoding,
    // so we optimize this case. You don't need to drop the `NA`s
    // out of the index before slicing (there aren't any), and you
    // can use a compact-seq for the assignment index.
    r_obj* index = compact_seq(0, x_size, true);
    r_list_poke(indices, 0, index);
    r_list_poke(xs, 0, vec_slice_unsafe(to, loc));
  } else {
    const int* v_loc = r_int_cbegin(loc);

    r_obj* index = r_alloc_integer(size);
    r_list_poke(indices, 0, index);
    int* v_index = r_int_begin(index);

    for (r_ssize i = 0, j = 0; i < x_size && j < size; ++i) {
      const int elt = v_loc[i];
      v_index[j] = elt;
      j += (elt != r_globals.na_int);
    }

    r_list_poke(xs, 0, vec_slice_unsafe(to, index));

    // Now actually fill `index` for use with `list_combine()`
    for (r_ssize i = 0, j = 0; i < x_size && j < size; ++i) {
      const int elt = v_loc[i];
      v_index[j] = i + 1;
      j += (elt != r_globals.na_int);
    }
  }

  FREE(2);
  return out;
}

// Form `indices` when `to` is length 1
//
// This applies for all of these:
// - `to = 1`, wrapped into `to = list(1)`
// - `to = list(1)`
// - `to = list(vec)`, where `vec` is `x_size`
//
// When you just have 1 `to` value, any match maps straight to this `to` value,
// there is nothing to "reslice" like in the other cases so we get to skip that
// step.
//
// # Safety
//
// Expect that all type and size checks have been done.
static
r_obj* build_indices_for_single_to(r_obj* x, r_obj* from_flat) {
  r_obj* indices = KEEP(r_alloc_list(1));

  // Want `na_equal = true` for `vec_recode_values(x, from = NA, to = to)`
  const bool na_equal = true;

  // TODO: For a lower memory footprint, we could teach `list_combine()` how to
  // take `compact_condition` as `indices` elements and then give `vec_in()` an
  // option to return one of these. For 10 mil integer elements, this reduces
  // memory usage by 30mb and is ~10% faster. That also makes giving a "single
  // `to`" its own path even more compelling.
  //
  // Find locations where `to` will be utilized
  r_obj* index = vec_in(
    x,
    from_flat,
    na_equal,
    vec_args.empty,
    vec_args.empty,
    r_lazy_null
  );
  r_list_poke(indices, 0, index);

  FREE(1);
  return indices;
}

// Builds a map from `from_flat` into `from`
//
// # Example
//
// When `from` is a flat vector, you have as many `index`es as `from` elements,
// and `vec_match()` locations map directly to `index` locations.
//
// ```
// from = c(1, 3, 4, 2, 5)
// index_locs = c(0, 1, 2, 3, 4)
// ```
//
// When `from` is a list of vectors, you have as many `index`es as `from` vectors.
// We flatten `from` to perform the `vec_match()`, but then we need a little help
// to map the match locations back to the `from` vector's `index` location.
//
// ```
// from = list(c(1, 3), c(4, 2, 5))
// from_flat     = c(1, 3, 4, 2, 5)
// from_flat_map = c(0, 0, 1, 1, 1)
// index_locs = c(0, 1)
// ```
static
r_obj* build_from_flat_map(r_obj* from, r_ssize from_size) {
  r_obj* const* v_from = r_list_cbegin(from);

  // Cache sizes rather than calling `vec_size()` again, as that's
  // a little faster
  r_obj* from_sizes = KEEP(r_alloc_raw(from_size * sizeof(r_ssize)));
  r_ssize* v_from_sizes = r_raw_begin(from_sizes);

  r_ssize from_flat_map_size = 0;

  for (r_ssize i = 0; i < from_size; ++i) {
    r_obj* elt = v_from[i];
    const r_ssize elt_size = vec_size(elt);
    v_from_sizes[i] = elt_size;
    from_flat_map_size += elt_size;
  }

  r_obj* from_flat_map = KEEP(r_alloc_raw(from_flat_map_size * sizeof(r_ssize)));
  r_ssize* v_from_flat_map = r_raw_begin(from_flat_map);
  r_ssize k = 0;

  for (r_ssize i = 0; i < from_size; ++i) {
    const r_ssize elt_size = v_from_sizes[i];
    for (r_ssize j = 0; j < elt_size; ++j) {
      v_from_flat_map[k] = i;
      ++k;
    }
  }

  FREE(2);
  return from_flat_map;
}

// Repeats vector `to` to match `from_flat`
//
// # Example
//
// ```
// from = list(c(1, 3), c(2, 4, 5))
// to = c(0, 1)
// ```
//
// We flatten `from` and then repeat `to` to match its flattened form
// so they stay aligned.
//
// ```
// from_flat = c(1, 3, 2, 4, 5)
// to = c(0, 0, 1, 1, 1)
// ```
static
r_obj* build_repeated_to(r_obj* to, r_obj* from) {
  r_obj* from_sizes = KEEP(list_sizes(from, vec_args.empty, r_lazy_null));
  to = vec_rep_each(to, from_sizes, r_lazy_null, vec_args.empty, vec_args.empty);
  FREE(1);
  return to;
}

static
r_obj* ptype_finalize(
  r_obj* ptype,
  r_obj* to,
  r_obj* default_,
  bool to_as_list_of_vectors,
  struct vctrs_arg* p_to_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
) {
  if (ptype != r_null) {
    // Performs scalar checks and whatnot
    return vec_ptype_final(ptype, vec_args.ptype, error_call);
  }

  // Otherwise `ptype` is `NULL`, and we determine it from `to` and `default`
  r_keep_loc ptype_pi;
  KEEP_HERE(ptype, &ptype_pi);

  if (to_as_list_of_vectors) {
    // Use only `to` and `p_to_arg` first for best errors
    // Not finalising `ptype` yet in case we need to incorporate `default`!
    ptype = vec_ptype_common(
      to,
      r_null,
      PTYPE_FINALISE_false,
      S3_FALLBACK_DEFAULT,
      p_to_arg,
      error_call
    );
    KEEP_AT(ptype, ptype_pi);

    // Now incorporate `default` and `p_default_arg`
    int _;
    ptype = vec_ptype2_params(default_, ptype, p_default_arg, vec_args.empty, error_call, &_);
    KEEP_AT(ptype, ptype_pi);
  } else {
    int _;
    ptype = vec_ptype2_params(to, default_, p_to_arg, p_default_arg, error_call, &_);
    KEEP_AT(ptype, ptype_pi);
  }

  // Finalize on the way out
  ptype = vec_ptype_finalise(ptype);

  FREE(1);
  return ptype;
}
