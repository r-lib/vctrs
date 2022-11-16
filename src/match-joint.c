#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/match-joint-decl.h"


#define VEC_JOINT_XTFRM_LOOP(CMP) do {                         \
  while (i < x_n_groups && j < y_n_groups) {                   \
    const int x_group_size = v_x_group_sizes[i];               \
    const int y_group_size = v_y_group_sizes[j];               \
                                                               \
    const int x_loc = v_x_o[x_o_loc] - 1;                      \
    const int y_loc = v_y_o[y_o_loc] - 1;                      \
                                                               \
    const int cmp = CMP(                                       \
      p_x_vec, x_loc,                                          \
      p_y_vec, y_loc,                                          \
      nan_distinct                                             \
    );                                                         \
                                                               \
    if (cmp == -1) {                                           \
      for (int k = 0; k < x_group_size; ++k) {                 \
        v_x_ranks[v_x_o[x_o_loc] - 1] = rank;                  \
        ++x_o_loc;                                             \
      }                                                        \
      ++i;                                                     \
    } else if (cmp == 1) {                                     \
      for (int k = 0; k < y_group_size; ++k) {                 \
        v_y_ranks[v_y_o[y_o_loc] - 1] = rank;                  \
        ++y_o_loc;                                             \
      }                                                        \
      ++j;                                                     \
    } else {                                                   \
      for (int k = 0; k < x_group_size; ++k) {                 \
        v_x_ranks[v_x_o[x_o_loc] - 1] = rank;                  \
        ++x_o_loc;                                             \
      }                                                        \
      for (int k = 0; k < y_group_size; ++k) {                 \
        v_y_ranks[v_y_o[y_o_loc] - 1] = rank;                  \
        ++y_o_loc;                                             \
      }                                                        \
      ++i;                                                     \
      ++j;                                                     \
    }                                                          \
                                                               \
    ++rank;                                                    \
  }                                                            \
} while(0)


/*
 * `vec_joint_xtfrm()` takes two vectors of the same type and computes an
 * xtfrm-like integer proxy for each that takes into account the values between
 * the two columns. It is approximately equal to the idea of:
 * `vec_rank(vec_c(x, y), ties = "dense")`
 * followed by splitting the ranks back up into two vectors matching the sizes
 * of x and y. The reason we don't do that is because it limits the maximum size
 * that `vec_locate_matches()` can work on to
 * `vec_size(x) + vec_size(y) <= INT_MAX`,
 * since you have to combine the vectors together.
 *
 * The sole purpose of this function is to support `vec_locate_matches()`.
 *
 * # For example:
 * x <- c(2, 1.5, 1)
 * y <- c(3, 1.2, 2)
 * # vec_joint_xtfrm(x, y) theoretically results in:
 * x <- c(4L, 3L, 1L)
 * y <- c(5L, 2L, 4L)
 * # While the above result is the general idea, we actually start counting
 * # from `INT_MIN + 1` to maximally utilize the `int` space while still
 * # avoiding `INT_MIN == NA_INTEGER`. So the result is really:
 * x <- c(-2147483644L, -2147483645L, -2147483647L)
 * y <- c(-2147483643L, -2147483646L, -2147483644L)
 */
// [[ include("match-joint.h") ]]
r_obj* vec_joint_xtfrm(r_obj* x,
                       r_obj* y,
                       r_ssize x_size,
                       r_ssize y_size,
                       bool nan_distinct,
                       r_obj* chr_proxy_collate) {
  int n_prot = 0;

  r_obj* out = KEEP_N(r_alloc_list(2), &n_prot);

  // These aren't true ranks, but that name makes the most sense
  r_obj* x_ranks = r_alloc_integer(x_size);
  r_list_poke(out, 0, x_ranks);
  int* v_x_ranks = r_int_begin(x_ranks);

  r_obj* y_ranks = r_alloc_integer(y_size);
  r_list_poke(out, 1, y_ranks);
  int* v_y_ranks = r_int_begin(y_ranks);

  // Retain the results of applying the proxy, normalizing the encoding, and
  // doing the collation transform, since we will make comparisons directly
  // on these objects.
  // This also uses a special variant of `vec_proxy_order()` to support list
  // columns, which have proxies that can't be computed independently.
  r_obj* proxies = KEEP_N(vec_joint_proxy_order(x, y), &n_prot);

  r_obj* x_proxy = r_list_get(proxies, 0);
  x_proxy = KEEP_N(vec_normalize_encoding(x_proxy), &n_prot);
  x_proxy = KEEP_N(proxy_apply_chr_proxy_collate(x_proxy, chr_proxy_collate), &n_prot);

  r_obj* y_proxy = r_list_get(proxies, 1);
  y_proxy = KEEP_N(vec_normalize_encoding(y_proxy), &n_prot);
  y_proxy = KEEP_N(proxy_apply_chr_proxy_collate(y_proxy, chr_proxy_collate), &n_prot);

  // Called with `direction = "asc", na_value = "smallest"` to match the
  // comparison helpers in `match-compare.h`
  r_obj* x_info = KEEP_N(vec_order_info(
    x_proxy,
    chrs_asc,
    chrs_smallest,
    nan_distinct,
    r_null,
    false
  ), &n_prot);

  r_obj* y_info = KEEP_N(vec_order_info(
    y_proxy,
    chrs_asc,
    chrs_smallest,
    nan_distinct,
    r_null,
    false
  ), &n_prot);

  const int* v_x_o = r_int_cbegin(r_list_get(x_info, 0));
  const int* v_x_group_sizes = r_int_cbegin(r_list_get(x_info, 1));
  r_ssize x_n_groups = r_length(r_list_get(x_info, 1));

  const int* v_y_o = r_int_cbegin(r_list_get(y_info, 0));
  const int* v_y_group_sizes = r_int_cbegin(r_list_get(y_info, 1));
  r_ssize y_n_groups = r_length(r_list_get(y_info, 1));

  const enum vctrs_type type = vec_proxy_typeof(x_proxy);

  const struct poly_vec* p_x_poly = new_poly_vec(x_proxy, type);
  KEEP_N(p_x_poly->shelter, &n_prot);
  const void* p_x_vec = p_x_poly->p_vec;

  const struct poly_vec* p_y_poly = new_poly_vec(y_proxy, type);
  KEEP_N(p_y_poly->shelter, &n_prot);
  const void* p_y_vec = p_y_poly->p_vec;

  r_ssize i = 0;
  r_ssize j = 0;
  r_ssize x_o_loc = 0;
  r_ssize y_o_loc = 0;

  // Start rank as small as possible (while still different from NA),
  // to maximally utilize `int` storage
  int rank = INT_MIN + 1;

  // Now that we have the ordering of both vectors,
  // it is just a matter of merging two sorted arrays
  switch (type) {
  case VCTRS_TYPE_logical: VEC_JOINT_XTFRM_LOOP(p_lgl_order_compare_na_equal); break;
  case VCTRS_TYPE_integer: VEC_JOINT_XTFRM_LOOP(p_int_order_compare_na_equal); break;
  case VCTRS_TYPE_double: VEC_JOINT_XTFRM_LOOP(p_dbl_order_compare_na_equal); break;
  case VCTRS_TYPE_complex: VEC_JOINT_XTFRM_LOOP(p_cpl_order_compare_na_equal); break;
  case VCTRS_TYPE_character: VEC_JOINT_XTFRM_LOOP(p_chr_order_compare_na_equal); break;
  case VCTRS_TYPE_dataframe: VEC_JOINT_XTFRM_LOOP(p_df_order_compare_na_equal); break;
  default: stop_unimplemented_vctrs_type("vec_joint_xtfrm", type);
  }

  while (i < x_n_groups) {
    // Finish up remaining x groups
    const int x_group_size = v_x_group_sizes[i];

    for (int k = 0; k < x_group_size; ++k) {
      v_x_ranks[v_x_o[x_o_loc] - 1] = rank;
      ++x_o_loc;
    }

    ++i;
    ++rank;
  }

  while (j < y_n_groups) {
    // Finish up remaining y groups
    const int y_group_size = v_y_group_sizes[j];

    for (int k = 0; k < y_group_size; ++k) {
      v_y_ranks[v_y_o[y_o_loc] - 1] = rank;
      ++y_o_loc;
    }

    ++j;
    ++rank;
  }

  FREE(n_prot);
  return out;
}


#undef VEC_JOINT_XTFRM_LOOP

// -----------------------------------------------------------------------------

/*
 * Specialized internal variant of `vec_proxy_order()` used in
 * `vec_joint_xtfrm()`.
 *
 * If we know that the `vec_proxy_order()` method of a type doesn't depend on
 * the data itself, then we just call `vec_proxy_order()` on `x` and `y`
 * separately. We know this is true for most base types (except lists) and
 * for the base R S3 types that we support natively in vctrs, so those get a
 * fast path.
 *
 * Otherwise, it is possible that the `vec_proxy_order()` method is dependent
 * on the data itself, like it is with lists and the bignum classes, so we need
 * to compute the order proxy "jointly" by combining `x` and `y` together.
 *
 * For example
 * x <- list(1.5, 2)
 * y <- list(2, 1.5)
 * vec_proxy_order(x)
 * # [1] 1 2
 * vec_proxy_order(y) # can't compare proxies when taken individually
 * # [1] 1 2
 * vec_proxy_order(c(x, y)) # jointly comparable
 * # [1] 1 2 2 1
 *
 * Combining `x` and `y` has the downsides that it:
 * - Is slower than the independent proxy method
 * - Limits the maximum data size to `vec_size(x) + vec_size(y) <= INT_MAX`
 *
 * Data frames are analyzed one column at a time, so if one of the columns
 * requires a joint proxy, then we only have to combine those individual columns
 * together rather than the entire data frames.
 */
static inline
r_obj* vec_joint_proxy_order(r_obj* x, r_obj* y) {
  if (r_typeof(x) != r_typeof(y)) {
    r_stop_internal("`x` and `y` should have the same type.");
  }

  switch (vec_typeof(x)) {
  case VCTRS_TYPE_unspecified:
  case VCTRS_TYPE_logical:
  case VCTRS_TYPE_integer:
  case VCTRS_TYPE_double:
  case VCTRS_TYPE_complex:
  case VCTRS_TYPE_character:
  case VCTRS_TYPE_raw: {
    return vec_joint_proxy_order_independent(x, y);
  }
  case VCTRS_TYPE_list: {
    return vec_joint_proxy_order_dependent(x, y);
  }
  case VCTRS_TYPE_dataframe: {
    return df_joint_proxy_order(x, y);
  }
  case VCTRS_TYPE_s3: {
    return vec_joint_proxy_order_s3(x, y);
  }
  case VCTRS_TYPE_null:
  case VCTRS_TYPE_scalar: {
    stop_unimplemented_vctrs_type("vec_joint_proxy_order", vec_typeof(x));
  }
  }

  r_stop_unreachable();
}

static inline
r_obj* vec_joint_proxy_order_independent(r_obj* x, r_obj* y) {
  r_obj* out = KEEP(r_alloc_list(2));

  r_list_poke(out, 0, vec_proxy_order(x));
  r_list_poke(out, 1, vec_proxy_order(y));

  FREE(1);
  return out;
}

static inline
r_obj* vec_joint_proxy_order_dependent(r_obj* x, r_obj* y) {
  r_ssize x_size = vec_size(x);
  r_ssize y_size = vec_size(y);

  r_obj* x_slicer = KEEP(compact_seq(0, x_size, true));
  r_obj* y_slicer = KEEP(compact_seq(x_size, y_size, true));

  r_obj* ptype = KEEP(vec_ptype(x, vec_args.empty, r_lazy_null));

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, x);
  r_list_poke(out, 1, y);

  // Combine
  // NOTE: Without long vector support, this limits the maximum allowed
  // size of `vec_locate_matches()` input to
  // `vec_size(x) + vec_size(y) <= INT_MAX`
  // when foreign columns are used.
  r_obj* combined = KEEP(vec_c(
    out,
    ptype,
    r_null,
    p_no_repair_opts,
    vec_args.empty,
    r_lazy_null
  ));

  // Compute joint order-proxy
  r_obj* proxy = KEEP(vec_proxy_order(combined));

  // Separate and store back in `out`
  r_list_poke(out, 0, vec_slice_unsafe(proxy, x_slicer));
  r_list_poke(out, 1, vec_slice_unsafe(proxy, y_slicer));

  FREE(6);
  return out;
}

static inline
r_obj* vec_joint_proxy_order_s3(r_obj* x, r_obj* y) {
  const enum vctrs_class_type type = class_type(x);

  if (type != class_type(y)) {
    r_stop_internal("`x` and `y` should have the same class type.");
  }

  switch (type) {
  case VCTRS_CLASS_bare_factor:
  case VCTRS_CLASS_bare_ordered:
  case VCTRS_CLASS_bare_date:
  case VCTRS_CLASS_bare_posixct:
  case VCTRS_CLASS_bare_posixlt: {
    return vec_joint_proxy_order_independent(x, y);
  }
  case VCTRS_CLASS_bare_asis:
  case VCTRS_CLASS_list:
  case VCTRS_CLASS_unknown: {
    return vec_joint_proxy_order_dependent(x, y);
  }
  case VCTRS_CLASS_bare_tibble:
  case VCTRS_CLASS_data_frame: {
    return df_joint_proxy_order(x, y);
  }
  case VCTRS_CLASS_bare_data_frame: {
    r_stop_internal("Bare data frames should have been handled earlier.");
  }
  case VCTRS_CLASS_none: {
    r_stop_internal("Unclassed objects should have been handled earlier.");
  }
  }

  r_stop_unreachable();
}

static inline
r_obj* df_joint_proxy_order(r_obj* x, r_obj* y) {
  x = KEEP(r_clone_referenced(x));
  y = KEEP(r_clone_referenced(y));

  const r_ssize n_cols = r_length(x);
  if (n_cols != r_length(y)) {
    r_stop_internal("`x` and `y` must have the same number of columns.");
  }

  r_obj* const* v_x = r_list_cbegin(x);
  r_obj* const* v_y = r_list_cbegin(y);

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* proxies = vec_joint_proxy_order(v_x[i], v_y[i]);
    r_list_poke(x, i, r_list_get(proxies, 0));
    r_list_poke(y, i, r_list_get(proxies, 1));
  }

  x = KEEP(df_flatten(x));
  x = KEEP(vec_proxy_unwrap(x));

  y = KEEP(df_flatten(y));
  y = KEEP(vec_proxy_unwrap(y));

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, x);
  r_list_poke(out, 1, y);

  FREE(7);
  return out;
}
