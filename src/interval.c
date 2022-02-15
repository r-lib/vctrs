#include "vctrs.h"
#include "order.h"
#include "complete.h"
#include "translate.h"
#include "poly-op.h"

enum vctrs_interval_missing {
  VCTRS_INTERVAL_MISSING_group = 0,
  VCTRS_INTERVAL_MISSING_drop = 1
};

#include "decl/interval-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* ffi_interval_groups(r_obj* start,
                           r_obj* end,
                           r_obj* ffi_abutting,
                           r_obj* ffi_missing) {
  const bool abutting = r_arg_as_bool(ffi_abutting, "abutting");
  const enum vctrs_interval_missing missing = parse_missing(ffi_missing);
  const bool locations = false;

  r_obj* out = KEEP(vec_interval_group_info(start, end, abutting, missing, locations));

  r_obj* loc_start = r_list_get(out, 0);
  r_obj* loc_end = r_list_get(out, 1);

  r_list_poke(out, 0, vec_slice_unsafe(start, loc_start));
  r_list_poke(out, 1, vec_slice_unsafe(end, loc_end));

  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* ffi_interval_locate_groups(r_obj* start,
                                  r_obj* end,
                                  r_obj* ffi_abutting,
                                  r_obj* ffi_missing) {
  const bool abutting = r_arg_as_bool(ffi_abutting, "abutting");
  const enum vctrs_interval_missing missing = parse_missing(ffi_missing);
  const bool locations = true;

  r_obj* out = KEEP(vec_interval_group_info(start, end, abutting, missing, locations));

  r_obj* key = r_list_get(out, 0);
  r_obj* loc_start = r_list_get(key, 0);
  r_obj* loc_end = r_list_get(key, 1);

  r_list_poke(key, 0, vec_slice_unsafe(start, loc_start));
  r_list_poke(key, 1, vec_slice_unsafe(end, loc_end));

  FREE(1);
  return out;
}

/*
 * If `locations = false`, returns a two column data frame containing a
 * `$start` column with locations to slice `start` with and an `$end` column
 * containing locations to slice `end` with. After slicing, the newly
 * generated intervals represent the "groups".
 *
 * If `locations = true`, returns a two column data frame containing a
 * `$key` column that holds the data frame generated by `locations = false`
 * and a `$loc` column that is a list-column of integer vectors that map each
 * interval defined by `[start, end)` to its corresponding group.
 *
 * We don't slice `start` and `end` here because it is often useful to just
 * know the locations, for example in `vec_interval_complement()`.
 */
static
r_obj* vec_interval_group_info(r_obj* start,
                               r_obj* end,
                               bool abutting,
                               enum vctrs_interval_missing missing,
                               bool locations) {
  int n_prot = 0;

  int _;
  r_obj* ptype = vec_ptype2_params(
    start,
    end,
    args_start,
    args_end,
    DF_FALLBACK_quiet,
    &_
  );
  KEEP_N(ptype, &n_prot);

  start = vec_cast_params(
    start,
    ptype,
    args_start,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  );
  KEEP_N(start, &n_prot);

  end = vec_cast_params(
    end,
    ptype,
    args_end,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  );
  KEEP_N(end, &n_prot);

  r_obj* start_proxy = KEEP_N(vec_proxy_compare(start), &n_prot);
  start_proxy = KEEP_N(vec_normalize_encoding(start_proxy), &n_prot);

  r_obj* end_proxy = KEEP_N(vec_proxy_compare(end), &n_prot);
  end_proxy = KEEP_N(vec_normalize_encoding(end_proxy), &n_prot);

  const enum vctrs_type type_proxy = vec_proxy_typeof(start_proxy);

  struct poly_vec* p_poly_start = new_poly_vec(start_proxy, type_proxy);
  PROTECT_POLY_VEC(p_poly_start, &n_prot);
  const void* p_start = p_poly_start->p_vec;

  struct poly_vec* p_poly_end = new_poly_vec(end_proxy, type_proxy);
  PROTECT_POLY_VEC(p_poly_end, &n_prot);
  const void* p_end = p_poly_end->p_vec;

  const poly_binary_int_fn_ptr fn_compare = new_poly_p_compare_na_equal(type_proxy);
  const poly_unary_bool_fn_ptr fn_is_missing = new_poly_p_is_missing(type_proxy);

  const r_ssize size = vec_size(start_proxy);

  if (size != vec_size(end_proxy)) {
    r_abort("`start` and `end` must have the same size.");
  }

  // Order is computed as ascending order, placing missing intervals up front
  // as the "smallest" values. We document that we assume that if `start` is
  // missing, then `end` is missing too.
  r_obj* order = KEEP_N(interval_order(start_proxy, end_proxy, size), &n_prot);
  const int* v_order = r_int_cbegin(order);

  // Assume the intervals can be merged into half their original size.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_loc_start = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_loc_start->shelter, &n_prot);

  struct r_dyn_array* p_loc_end = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_loc_end->shelter, &n_prot);

  struct r_dyn_array* p_loc = NULL;
  if (locations) {
    p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
    KEEP_N(p_loc->shelter, &n_prot);
  }

  r_ssize i = 0;

  r_ssize loc_order_missing_start = 0;
  r_ssize loc_order_missing_end = -1;

  // Move `i` past any missing intervals (they are at the front),
  // recording last missing interval location for later. Only need to check
  // missingness of `start`, because we document that we assume that `end`
  // is missing if `start` is missing.
  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    if (!fn_is_missing(p_start, loc)) {
      break;
    }

    loc_order_missing_end = i;
  }

  r_ssize loc_order_start = 0;
  r_ssize loc_order_end = -1;

  r_ssize loc_group_start = 0;
  r_ssize loc_group_end = -1;

  if (i < size) {
    // Set information about first usable interval
    const r_ssize loc = v_order[i] - 1;
    loc_order_start = i;
    loc_order_end = i;
    loc_group_start = loc;
    loc_group_end = loc;
    ++i;
  }

  const int merge_limit = abutting ? -1 : 0;

  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    // If `abutting`, this says: if group end < new start, finish out the group
    // If `!abutting`, this says: if group end <= new start, finish out the group
    if (fn_compare(p_end, loc_group_end, p_start, loc) <= merge_limit) {
      r_dyn_int_push_back(p_loc_start, loc_group_start + 1);
      r_dyn_int_push_back(p_loc_end, loc_group_end + 1);

      if (locations) {
        const r_ssize loc_size = loc_order_end - loc_order_start + 1;

        r_obj* loc = r_new_integer(loc_size);
        r_dyn_list_push_back(p_loc, loc);
        int* v_loc = r_int_begin(loc);

        const int* v_order_start = v_order + loc_order_start;
        memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
      }

      loc_order_start = loc_order_end + 1;
      loc_group_start = loc;
      loc_group_end = loc;
    } else if (fn_compare(p_end, loc_group_end, p_end, loc) == -1) {
      loc_group_end = loc;
    }

    loc_order_end = i;
  }

  if (loc_order_end >= loc_order_start) {
    // Log last interval
    r_dyn_int_push_back(p_loc_start, loc_group_start + 1);
    r_dyn_int_push_back(p_loc_end, loc_group_end + 1);

    if (locations) {
      const r_ssize loc_size = loc_order_end - loc_order_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_dyn_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  if (missing == VCTRS_INTERVAL_MISSING_group &&
      loc_order_missing_end >= loc_order_missing_start) {
    // Log missing interval at the end
    const r_ssize loc_group_missing_start = v_order[loc_order_missing_start] - 1;
    const r_ssize loc_group_missing_end = v_order[loc_order_missing_end] - 1;

    r_dyn_int_push_back(p_loc_start, loc_group_missing_start + 1);
    r_dyn_int_push_back(p_loc_end, loc_group_missing_end + 1);

    if (locations) {
      const r_ssize loc_size = loc_order_missing_end - loc_order_missing_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_dyn_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_missing_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  r_obj* key = KEEP_N(r_new_list(2), &n_prot);
  r_list_poke(key, 0, r_dyn_unwrap(p_loc_start));
  r_list_poke(key, 1, r_dyn_unwrap(p_loc_end));

  r_obj* key_names = r_new_character(2);
  r_attrib_poke_names(key, key_names);
  r_chr_poke(key_names, 0, r_str("start"));
  r_chr_poke(key_names, 1, r_str("end"));

  r_init_data_frame(key, p_loc_start->count);

  r_obj* out = r_null;
  r_keep_loc out_shelter;
  KEEP_HERE(out, &out_shelter);
  ++n_prot;

  if (locations) {
    out = r_new_list(2);
    KEEP_AT(out, out_shelter);
    r_list_poke(out, 0, key);
    r_list_poke(out, 1, r_dyn_unwrap(p_loc));

    r_obj* out_names = r_new_character(2);
    r_attrib_poke_names(out, out_names);
    r_chr_poke(out_names, 0, r_str("key"));
    r_chr_poke(out_names, 1, r_str("loc"));

    r_init_data_frame(out, p_loc_start->count);
  } else {
    out = key;
  }

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* ffi_interval_complement(r_obj* start,
                               r_obj* end,
                               r_obj* lower,
                               r_obj* upper) {
  return vec_interval_complement(start, end, lower, upper);
}

static
r_obj* vec_interval_complement(r_obj* start,
                               r_obj* end,
                               r_obj* lower,
                               r_obj* upper) {
  int n_prot = 0;

  int _;
  r_obj* ptype = vec_ptype2_params(
    start,
    end,
    args_start,
    args_end,
    DF_FALLBACK_quiet,
    &_
  );
  KEEP_N(ptype, &n_prot);

  start = vec_cast_params(
    start,
    ptype,
    args_start,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  );
  KEEP_N(start, &n_prot);

  end = vec_cast_params(
    end,
    ptype,
    args_end,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  );
  KEEP_N(end, &n_prot);

  r_obj* start_proxy = KEEP_N(vec_proxy_compare(start), &n_prot);
  start_proxy = KEEP_N(vec_normalize_encoding(start_proxy), &n_prot);

  r_obj* end_proxy = KEEP_N(vec_proxy_compare(end), &n_prot);
  end_proxy = KEEP_N(vec_normalize_encoding(end_proxy), &n_prot);

  const enum vctrs_type type_proxy = vec_proxy_typeof(start_proxy);

  struct poly_vec* p_poly_start = new_poly_vec(start_proxy, type_proxy);
  PROTECT_POLY_VEC(p_poly_start, &n_prot);
  const void* p_start = p_poly_start->p_vec;

  struct poly_vec* p_poly_end = new_poly_vec(end_proxy, type_proxy);
  PROTECT_POLY_VEC(p_poly_end, &n_prot);
  const void* p_end = p_poly_end->p_vec;

  const poly_binary_int_fn_ptr fn_compare = new_poly_p_compare_na_equal(type_proxy);

  bool use_lower = (lower != r_null);
  bool use_upper = (upper != r_null);

  bool append_lower = false;
  bool append_upper = false;

  const void* p_lower = NULL;
  if (use_lower) {
    if (vec_size(lower) != 1) {
      r_abort("`lower` must be size 1.");
    }

    lower = vec_cast_params(
      lower,
      ptype,
      args_lower,
      args_empty,
      DF_FALLBACK_quiet,
      S3_FALLBACK_false
    );
    KEEP_N(lower, &n_prot);

    r_obj* lower_proxy = KEEP_N(vec_proxy_compare(lower), &n_prot);
    lower_proxy = KEEP_N(vec_normalize_encoding(lower_proxy), &n_prot);

    r_obj* lower_complete = KEEP_N(vec_detect_complete(lower_proxy), &n_prot);
    if (!r_lgl_get(lower_complete, 0)) {
      r_abort("`lower` can't contain missing values.");
    }

    struct poly_vec* p_poly_lower = new_poly_vec(lower_proxy, type_proxy);
    PROTECT_POLY_VEC(p_poly_lower, &n_prot);
    p_lower = p_poly_lower->p_vec;
  }

  const void* p_upper = NULL;
  if (use_upper) {
    if (vec_size(upper) != 1) {
      r_abort("`upper` must be size 1.");
    }

    upper = vec_cast_params(
      upper,
      ptype,
      args_upper,
      args_empty,
      DF_FALLBACK_quiet,
      S3_FALLBACK_false
    );
    KEEP_N(upper, &n_prot);

    r_obj* upper_proxy = KEEP_N(vec_proxy_compare(upper), &n_prot);
    upper_proxy = KEEP_N(vec_normalize_encoding(upper_proxy), &n_prot);

    r_obj* upper_complete = KEEP_N(vec_detect_complete(upper_proxy), &n_prot);
    if (!r_lgl_get(upper_complete, 0)) {
      r_abort("`upper` can't contain missing values.");
    }

    struct poly_vec* p_poly_upper = new_poly_vec(upper_proxy, type_proxy);
    PROTECT_POLY_VEC(p_poly_upper, &n_prot);
    p_upper = p_poly_upper->p_vec;
  }

  if (use_lower && use_upper && fn_compare(p_lower, 0, p_upper, 0) >= 0) {
    // Handle the special case of `lower >= upper` up front.
    // This could also be an error, but we try to be a little flexible.
    // These can't follow the standard code path because it assumes
    // `lower < upper`, like the rest of the intervals.
    // - `lower > upper` is an invalid interval.
    // - `lower = upper` will always result in an empty complement.
    r_obj* out = KEEP_N(r_new_list(2), &n_prot);
    r_list_poke(out, 0, vec_slice_unsafe(start, vctrs_shared_empty_int));
    r_list_poke(out, 1, vec_slice_unsafe(end, vctrs_shared_empty_int));

    r_obj* out_names = r_new_character(2);
    r_attrib_poke_names(out, out_names);
    r_chr_poke(out_names, 0, r_str("start"));
    r_chr_poke(out_names, 1, r_str("end"));

    r_init_data_frame(out, 0);

    FREE(n_prot);
    return out;
  }

  // Merge to sort, remove all missings, and merge all abutting intervals
  const bool abutting = true;
  const bool locations = false;
  r_obj* minimal = KEEP_N(vec_interval_group_info(
    start,
    end,
    abutting,
    VCTRS_INTERVAL_MISSING_drop,
    locations
  ), &n_prot);
  const int* v_loc_minimal_start = r_int_cbegin(r_list_get(minimal, 0));
  const int* v_loc_minimal_end = r_int_cbegin(r_list_get(minimal, 1));

  r_ssize size = vec_size(minimal);

  // Because we have the minimal interval information (i.e. no intervals overlap
  // or abut!), we know that the complement takes exactly `size - 1` space if
  // `lower` and `upper` aren't used.
  //
  // If `lower` is used, it can at most add one more interval, and
  // requires one more `loc_end` location. No `loc_start` location is needed
  // because we just append `lower` to the front if needed.
  //
  // If `upper` is used, it can at most add one more interval, and
  // requires one more `loc_start` location. No `loc_end` location is needed
  // because we just append `upper` to the end if needed.
  const r_ssize max_size_start = r_ssize_max(size - 1 + use_upper, 0);
  const r_ssize max_size_end = r_ssize_max(size - 1 + use_lower, 0);

  r_obj* loc_start = KEEP_N(r_alloc_integer(max_size_start), &n_prot);
  int* v_loc_start = r_int_begin(loc_start);
  r_ssize i_start = 0;

  r_obj* loc_end = KEEP_N(r_alloc_integer(max_size_end), &n_prot);
  int* v_loc_end = r_int_begin(loc_end);
  r_ssize i_end = 0;

  r_ssize i = 0;

  r_ssize loc_lower_is_after_start_of = -1;
  r_ssize loc_lower_is_before_end_of = 0;

  if (use_lower) {
    // Shift `i` forward to the first interval completely past `lower`.
    // Track information about where `lower` is in relation to the intervals.
    for (; i < size; ++i) {
      const r_ssize loc_start = v_loc_minimal_start[i] - 1;
      const r_ssize loc_end = v_loc_minimal_end[i] - 1;

      if (fn_compare(p_lower, 0, p_end, loc_end) == 1) {
        ++loc_lower_is_before_end_of;
        ++loc_lower_is_after_start_of;
      } else if (fn_compare(p_lower, 0, p_start, loc_start) >= 0) {
        ++loc_lower_is_after_start_of;
      } else {
        break;
      }
    }
  }

  r_ssize loc_upper_is_after_start_of = size - 1;
  r_ssize loc_upper_is_before_end_of = size;

  if (use_upper) {
    // Shift `size` backwards to the first interval that is completely before `upper`.
    // Track information about where `upper` is in relation to the intervals.
    for (; size - 1 >= 0; --size) {
      const r_ssize loc_start = v_loc_minimal_start[size - 1] - 1;
      const r_ssize loc_end = v_loc_minimal_end[size - 1] - 1;

      if (fn_compare(p_upper, 0, p_start, loc_start) == -1) {
        --loc_upper_is_before_end_of;
        --loc_upper_is_after_start_of;
      } else if (fn_compare(p_upper, 0, p_end, loc_end) <= 0) {
        --loc_upper_is_before_end_of;
      } else {
        break;
      }
    }
  }

  const bool has_intervals_between = i < size;

  if (use_lower && has_intervals_between) {
    r_ssize loc_gap_start = -1;
    if (loc_lower_is_before_end_of == loc_lower_is_after_start_of) {
      // `lower` is in the middle of an interval, use the end of that interval
      loc_gap_start = v_loc_minimal_end[loc_lower_is_before_end_of] - 1;
    } else {
      // `lower` is not within an interval, use `lower`
      append_lower = true;
    }

    // The next start location is the end of the interval that `loc_gap_start`
    // lines up with. We know this start location exists because of
    // `has_intervals_between`.
    const r_ssize loc_gap_end = v_loc_minimal_start[loc_lower_is_after_start_of + 1] - 1;

    if (!append_lower) {
      v_loc_start[i_start] = loc_gap_start + 1;
      ++i_start;
    }
    v_loc_end[i_end] = loc_gap_end + 1;
    ++i_end;
  }

  r_ssize loc_previous_end = -1;

  if (i < size) {
    // Set information about first usable interval
    loc_previous_end = v_loc_minimal_end[i] - 1;
    ++i;
  }

  for (; i < size; ++i) {
    const r_ssize loc_elt_start = v_loc_minimal_start[i] - 1;
    const r_ssize loc_elt_end = v_loc_minimal_end[i] - 1;

    const r_ssize loc_gap_start = loc_previous_end;
    const r_ssize loc_gap_end = loc_elt_start;

    v_loc_start[i_start] = loc_gap_start + 1;
    ++i_start;

    v_loc_end[i_end] = loc_gap_end + 1;
    ++i_end;

    loc_previous_end = loc_elt_end;
  }

  if (use_upper && has_intervals_between) {
    // The previous end location is the start of the interval that `loc_gap_end`
    // lines up with. We know this end location exists because of
    // `has_intervals_between`.
    const r_ssize loc_gap_start = v_loc_minimal_end[loc_upper_is_before_end_of - 1] - 1;

    r_ssize loc_gap_end = -1;
    if (loc_upper_is_before_end_of == loc_upper_is_after_start_of) {
      // `upper` is in the middle of an interval, use the start of that interval
      loc_gap_end = v_loc_minimal_start[loc_upper_is_before_end_of] - 1;
    } else {
      // `upper` is not within an interval, use `upper`
      append_upper = true;
    }

    v_loc_start[i_start] = loc_gap_start + 1;
    ++i_start;
    if (!append_upper) {
      v_loc_end[i_end] = loc_gap_end + 1;
      ++i_end;
    }
  }

  if (use_lower && use_upper && !has_intervals_between) {
    /*
     * This branch handles the case when `lower` and `upper` have no full
     * intervals between them. They can be in any of these states. In
     * particular, if they are in the same interval together, then there is
     * no complement.
     *
     * | [ ) <lower> <upper> [ ) | append_lower = append_upper = true. complement: <lower> -> <upper>
     * | [ <lower> ) <upper> [ ) | append_upper = true. complement: ) -> <upper>
     * | [ ) <lower> [ <upper> ) | append_lower = true. complement: <lower> -> [
     * | [ <lower> ) [ <upper> ) | both in separate intervals. complement: ) -> [
     * | [ <lower> <upper> ) [ ) | both in same interval! complement: none
     * | [ ) [ <lower> <upper> ) | both in same interval! complement: none
     */
    bool lower_in_interval = false;
    bool upper_in_interval = false;

    r_ssize loc_gap_start = -1;
    if (loc_lower_is_before_end_of == loc_lower_is_after_start_of) {
      lower_in_interval = true;
      loc_gap_start = v_loc_minimal_end[loc_lower_is_before_end_of] - 1;
    } else {
      append_lower = true;
    }

    r_ssize loc_gap_end = -1;
    if (loc_upper_is_before_end_of == loc_upper_is_after_start_of) {
      upper_in_interval = true;
      loc_gap_end = v_loc_minimal_start[loc_upper_is_before_end_of] - 1;
    } else {
      append_upper = true;
    }

    const bool lower_and_upper_in_same_interval =
      lower_in_interval &&
      upper_in_interval &&
      (loc_lower_is_before_end_of == loc_upper_is_before_end_of);

    if (!append_lower && !lower_and_upper_in_same_interval) {
      v_loc_start[i_start] = loc_gap_start + 1;
      ++i_start;
    }
    if (!append_upper && !lower_and_upper_in_same_interval) {
      v_loc_end[i_end] = loc_gap_end + 1;
      ++i_end;
    }
  }

  // This should essentially be free.
  // It will only ever shrink `loc_start` and `loc_end`.
  loc_start = KEEP_N(r_int_resize(loc_start, i_start), &n_prot);
  loc_end = KEEP_N(r_int_resize(loc_end, i_end), &n_prot);

  // Slice `end` to get new starts and `start` to get new ends!
  r_obj* out_start = KEEP_N(vec_slice_unsafe(end, loc_start), &n_prot);
  r_obj* out_end = KEEP_N(vec_slice_unsafe(start, loc_end), &n_prot);

  if (append_lower || append_upper) {
    r_obj* args = KEEP_N(r_new_list(2), &n_prot);

    const struct name_repair_opts name_repair_opts = {
      .type = name_repair_none,
      .fn = R_NilValue
    };

    if (append_lower) {
      // Push `lower` to the start of the new starts
      r_list_poke(args, 0, lower);
      r_list_poke(args, 1, out_start);

      out_start = KEEP_N(vec_c(
        args,
        ptype,
        R_NilValue,
        &name_repair_opts
      ), &n_prot);
    }

    if (append_upper) {
      // Push `upper` to the end of the new ends
      r_list_poke(args, 0, out_end);
      r_list_poke(args, 1, upper);

      out_end = KEEP_N(vec_c(
        args,
        ptype,
        R_NilValue,
        &name_repair_opts
      ), &n_prot);
    }
  }

  r_obj* out = KEEP_N(r_new_list(2), &n_prot);
  r_list_poke(out, 0, out_start);
  r_list_poke(out, 1, out_end);

  r_obj* out_names = r_new_character(2);
  r_attrib_poke_names(out, out_names);
  r_chr_poke(out_names, 0, r_str("start"));
  r_chr_poke(out_names, 1, r_str("end"));

  r_init_data_frame(out, vec_size(out_start));

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

/*
 * `interval_order()` orders the `start` and `end` values of a vector of
 * intervals in ascending order. It places missing intervals at the front.
 * We document that we make the assumption that if `start` is missing, then
 * `end` is also missing. We also document the assumption that partially missing
 * (i.e. incomplete but not missing) observations are not allowed in either
 * bound.
 */
static inline
r_obj* interval_order(r_obj* start, r_obj* end, r_ssize size) {
  // Put them in a data frame to compute joint ordering
  r_obj* df = KEEP(r_new_list(2));
  r_list_poke(df, 0, start);
  r_list_poke(df, 1, end);

  r_obj* df_names = r_new_character(2);
  r_attrib_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("start"));
  r_chr_poke(df_names, 1, r_str("end"));

  r_init_data_frame(df, size);

  const bool nan_distinct = false;
  r_obj* chr_proxy_collate = r_null;

  r_obj* out = vec_order(
    df,
    chrs_asc,
    chrs_smallest,
    nan_distinct,
    chr_proxy_collate
  );

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_interval_missing parse_missing(r_obj* missing) {
  if (!r_is_string(missing)) {
    r_abort("`missing` must be a string.");
  }

  const char* c_missing = r_chr_get_c_string(missing, 0);

  if (!strcmp(c_missing, "group")) return VCTRS_INTERVAL_MISSING_group;
  if (!strcmp(c_missing, "drop")) return VCTRS_INTERVAL_MISSING_drop;

  r_abort("`missing` must be either \"group\" or \"drop\".");
}

// -----------------------------------------------------------------------------

void vctrs_init_interval(r_obj* ns) {
  args_start_ = new_wrapper_arg(NULL, "start");
  args_end_ = new_wrapper_arg(NULL, "end");
  args_lower_ = new_wrapper_arg(NULL, "lower");
  args_upper_ = new_wrapper_arg(NULL, "upper");
}
