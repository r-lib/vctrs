#include "vctrs.h"
#include "order.h"
#include "compare.h"
#include "complete.h"
#include "translate.h"
#include "poly-op.h"

enum vctrs_interval_incomplete {
  VCTRS_INTERVAL_INCOMPLETE_merge = 0,
  VCTRS_INTERVAL_INCOMPLETE_drop = 1,
  VCTRS_INTERVAL_INCOMPLETE_error = 2
};

#include "decl/interval-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* ffi_locate_interval_merge_bounds(r_obj* start,
                                        r_obj* end,
                                        r_obj* ffi_abutting,
                                        r_obj* ffi_incomplete) {
  const bool abutting = r_arg_as_bool(ffi_abutting, "abutting");
  const enum vctrs_interval_incomplete incomplete = parse_incomplete(ffi_incomplete);
  const bool groups = false;
  return vec_locate_interval_merge_info(start, end, abutting, incomplete, groups);
}

// [[ register() ]]
r_obj* ffi_locate_interval_merge_groups(r_obj* start,
                                        r_obj* end,
                                        r_obj* ffi_abutting,
                                        r_obj* ffi_incomplete) {
  const bool abutting = r_arg_as_bool(ffi_abutting, "abutting");
  const enum vctrs_interval_incomplete incomplete = parse_incomplete(ffi_incomplete);
  const bool groups = true;
  return vec_locate_interval_merge_info(start, end, abutting, incomplete, groups);
}

static
r_obj* vec_locate_interval_merge_info(r_obj* start,
                                      r_obj* end,
                                      bool abutting,
                                      enum vctrs_interval_incomplete incomplete,
                                      bool groups) {
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

  const r_ssize size = vec_size(start_proxy);

  if (size != vec_size(end_proxy)) {
    r_abort("`start` and `end` must have the same size.");
  }

  /*
   * NA == (either incomplete), possibly allowed, but we don't check that here
   * -1 == (start < end), typical case
   * 0  == (start == end), not allowed (empty interval)
   * 1  == (start > end), not allowed
   *
   * Comparison loop relies on `NA` being the smallest integer value, so it
   * isn't flagged as being `>= 0`.
   */
  r_obj* compare = KEEP_N(vec_compare(start_proxy, end_proxy, false), &n_prot);
  const int* v_compare = r_int_cbegin(compare);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_compare[i] >= 0) {
      r_abort("`start` must be less than `end`.");
    }
  }

  /*
   * Joint completeness is used to determine how incomplete intervals should
   * be handled.
   *
   * We can't rely on the `NA`s produced by `vec_compare(na_equal = false)`,
   * because that only propagates an `NA` if the comparison hits a missing
   * value before it can be finalized.
   * i.e. this can be finalized early, but we want to treat it as incomplete:
   * vec_compare(data_frame(x = 1, y = NA), data_frame(x = 2, y = NA))
   */
  r_obj* complete = KEEP_N(interval_detect_complete(start, end), &n_prot);
  const int* v_complete = r_lgl_cbegin(complete);

  r_obj* order = KEEP_N(interval_order(complete, start, end), &n_prot);
  const int* v_order = r_int_cbegin(order);

  if (incomplete == VCTRS_INTERVAL_INCOMPLETE_error &&
      size > 0 &&
      !v_complete[v_order[0] - 1]) {
      // Incomplete intervals will be at the front of `order`
      r_abort("`start` and `end` can't contain missing values.");
  }

  // Assume the intervals can be merged into half their original size.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_loc_start = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_loc_start->shelter, &n_prot);

  struct r_dyn_array* p_loc_end = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_loc_end->shelter, &n_prot);

  struct r_dyn_array* p_loc = NULL;
  if (groups) {
    p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
    KEEP_N(p_loc->shelter, &n_prot);
  }

  r_ssize i = 0;

  r_ssize loc_order_incomplete_start = 0;
  r_ssize loc_order_incomplete_end = -1;

  // Move `i` past any incomplete intervals (they are at the front),
  // recording last incomplete interval location for later
  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    if (v_complete[loc]) {
      break;
    }

    loc_order_incomplete_end = i;
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

      if (groups) {
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

    if (groups) {
      const r_ssize loc_size = loc_order_end - loc_order_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_dyn_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  if (incomplete == VCTRS_INTERVAL_INCOMPLETE_merge &&
      loc_order_incomplete_end >= loc_order_incomplete_start) {
    // Log incomplete interval at the end.
    // Log with `NA_integer_` as the location so slicing the original
    // inputs consistently gives missing values.
    r_dyn_int_push_back(p_loc_start, r_globals.na_int);
    r_dyn_int_push_back(p_loc_end, r_globals.na_int);

    if (groups) {
      const r_ssize loc_size = loc_order_incomplete_end - loc_order_incomplete_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_dyn_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_incomplete_start;
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

  if (groups) {
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

/*
 * `interval_order()` orders the `start` and `end` values of a vector of
 * intervals, but also groups them by their `complete` value. We sort in
 * ascending order, so we end up with:
 *
 * - Incomplete intervals first (`complete == FALSE`)
 * - Then sorted typical intervals (`complete == TRUE`)
 * - Empty and invalid intervals have already been handled
 */
static inline
r_obj* interval_order(r_obj* complete, r_obj* start, r_obj* end) {
  // Put them in a data frame to compute joint ordering
  r_obj* df = KEEP(r_new_list(3));
  r_list_poke(df, 0, complete);
  r_list_poke(df, 1, start);
  r_list_poke(df, 2, end);

  r_obj* df_names = r_new_character(3);
  r_attrib_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("complete"));
  r_chr_poke(df_names, 1, r_str("start"));
  r_chr_poke(df_names, 2, r_str("end"));

  r_init_data_frame(df, r_length(complete));

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

static inline
r_obj* interval_detect_complete(r_obj* start, r_obj* end) {
  // Put them in a data frame to compute joint completeness
  r_obj* df = KEEP(r_new_list(2));
  r_list_poke(df, 0, start);
  r_list_poke(df, 1, end);

  r_obj* df_names = r_new_character(2);
  r_attrib_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("start"));
  r_chr_poke(df_names, 1, r_str("end"));

  r_init_data_frame(df, vec_size(start));

  r_obj* out = vec_detect_complete(df);

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_interval_incomplete parse_incomplete(r_obj* incomplete) {
  if (!r_is_string(incomplete)) {
    r_abort("`incomplete` must be a string.");
  }

  const char* c_incomplete = r_chr_get_c_string(incomplete, 0);

  if (!strcmp(c_incomplete, "merge")) return VCTRS_INTERVAL_INCOMPLETE_merge;
  if (!strcmp(c_incomplete, "drop")) return VCTRS_INTERVAL_INCOMPLETE_drop;
  if (!strcmp(c_incomplete, "error")) return VCTRS_INTERVAL_INCOMPLETE_error;

  r_abort("`incomplete` must be one of: \"merge\", \"drop\", or \"error\".");
}

// -----------------------------------------------------------------------------

void vctrs_init_interval(r_obj* ns) {
  args_start_ = new_wrapper_arg(NULL, "start");
  args_end_ = new_wrapper_arg(NULL, "end");
}
