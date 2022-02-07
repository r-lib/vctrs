#include "match-joint.h"
#include "vctrs.h"
#include "utils.h"
#include "translate.h"
#include "order.h"
#include "order-collate.h"
#include "match-compare.h"


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
    true
  ), &n_prot);

  r_obj* y_info = KEEP_N(vec_order_info(
    y_proxy,
    chrs_asc,
    chrs_smallest,
    nan_distinct,
    r_null,
    true
  ), &n_prot);

  const int* v_x_o = r_int_cbegin(r_list_get(x_info, 0));
  const int* v_x_group_sizes = r_int_cbegin(r_list_get(x_info, 1));
  r_ssize x_n_groups = r_length(r_list_get(x_info, 1));

  const int* v_y_o = r_int_cbegin(r_list_get(y_info, 0));
  const int* v_y_group_sizes = r_int_cbegin(r_list_get(y_info, 1));
  r_ssize y_n_groups = r_length(r_list_get(y_info, 1));

  const enum vctrs_type type = vec_proxy_typeof(x_proxy);

  const struct poly_vec* p_x_poly = new_poly_vec(x_proxy, type);
  PROTECT_POLY_VEC(p_x_poly, &n_prot);
  const void* p_x_vec = p_x_poly->p_vec;

  const struct poly_vec* p_y_poly = new_poly_vec(y_proxy, type);
  PROTECT_POLY_VEC(p_x_poly, &n_prot);
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
  case vctrs_type_logical: VEC_JOINT_XTFRM_LOOP(p_lgl_order_compare_na_equal); break;
  case vctrs_type_integer: VEC_JOINT_XTFRM_LOOP(p_int_order_compare_na_equal); break;
  case vctrs_type_double: VEC_JOINT_XTFRM_LOOP(p_dbl_order_compare_na_equal); break;
  case vctrs_type_complex: VEC_JOINT_XTFRM_LOOP(p_cpl_order_compare_na_equal); break;
  case vctrs_type_character: VEC_JOINT_XTFRM_LOOP(p_chr_order_compare_na_equal); break;
  case vctrs_type_dataframe: VEC_JOINT_XTFRM_LOOP(p_df_order_compare_na_equal); break;
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
