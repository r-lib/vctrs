#include "vctrs.h"
#include "order.h"
#include "translate.h"
#include "poly-op.h"

#include "decl/interval-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* ffi_locate_interval_merge_bounds(r_obj* start,
                                        r_obj* end,
                                        r_obj* ffi_abutting) {
  const bool abutting = r_arg_as_bool(ffi_abutting, "abutting");
  const bool groups = false;
  return vec_locate_interval_merge_info(start, end, abutting, groups);
}

// [[ register() ]]
r_obj* ffi_locate_interval_merge_groups(r_obj* start,
                                        r_obj* end,
                                        r_obj* ffi_abutting) {
  const bool abutting = r_arg_as_bool(ffi_abutting, "abutting");
  const bool groups = true;
  return vec_locate_interval_merge_info(start, end, abutting, groups);
}

static
r_obj* vec_locate_interval_merge_info(r_obj* start,
                                      r_obj* end,
                                      bool abutting,
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
  if (groups) {
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

  if (loc_order_missing_end >= loc_order_missing_start) {
    // Log missing interval at the end
    const r_ssize loc_group_missing_start = v_order[loc_order_missing_start] - 1;
    const r_ssize loc_group_missing_end = v_order[loc_order_missing_end] - 1;

    r_dyn_int_push_back(p_loc_start, loc_group_missing_start + 1);
    r_dyn_int_push_back(p_loc_end, loc_group_missing_end + 1);

    if (groups) {
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

void vctrs_init_interval(r_obj* ns) {
  args_start_ = new_wrapper_arg(NULL, "start");
  args_end_ = new_wrapper_arg(NULL, "end");
}
