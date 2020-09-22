/*
 * The implementation of vec_order() is based on data.table’s forder() and their
 * earlier contribution to R’s order(). See LICENSE.note for more information.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2020, RStudio
 * Copyright (c) 2020, Data table team
 */

#ifndef VCTRS_ORDER_RADIX_H
#define VCTRS_ORDER_RADIX_H

#include "vctrs.h"
#include "lazy.h"
#include "utils.h"
#include "order-groups.h"
#include "order-truelength.h"

// -----------------------------------------------------------------------------

/*
 * `order` is an integer vector intended to hold the ordering vector
 * in `vec_order()`. It is allocated eagerly, but the initialization of its
 * values is done lazily. Typically, it is initialized to a 1-based sequential
 * ordering which is rearranged by the internal algorithm. However, for the
 * counting order, the initialization is not required for the first integer
 * column, which can result in a nice performance improvement.
 */
struct order {
  SEXP self;
  SEXP data;
  int* p_data;
  r_ssize size;
  bool initialized;
};

#define PROTECT_ORDER(p_order, p_n) do { \
  PROTECT((p_order)->self);              \
  PROTECT((p_order)->data);              \
  *(p_n) += 2;                           \
} while (0)

static inline
struct order* new_order(r_ssize size) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct order)));
  struct order* p_order = (struct order*) RAW(self);

  SEXP data = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_data = INTEGER(data);

  p_order->self = self;
  p_order->data = data;
  p_order->p_data = p_data;
  p_order->size = size;
  p_order->initialized = false;

  UNPROTECT(2);
  return p_order;
}

static inline
int* init_order(struct order* p_order) {
  if (p_order->initialized) {
    return p_order->p_data;
  }

  // Initialize `x` with sequential 1-based ordering
  for (r_ssize i = 0; i < p_order->size; ++i) {
    p_order->p_data[i] = i + 1;
  }

  p_order->initialized = true;

  return p_order->p_data;
}

// -----------------------------------------------------------------------------

/*
 * `order_info` is a meta struct that holds all of the information used to
 * call `vec_order()`. It is returned from `vec_order_info()` and can be
 * used:
 * - For just the ordering in `p_order`
 * - For the additional group sizes in `p_group_infos`
 * - To compute the ordering by appearance by calling `vec_order_opts()`
 *   a second time with `p_order->data` as the input requiring ordering
 */
struct order_info {
  SEXP self;
  struct order* p_order;
  struct lazy_raw* p_lazy_x_chunk;
  struct lazy_raw* p_lazy_x_aux;
  struct lazy_raw* p_lazy_o_aux;
  struct lazy_raw* p_lazy_bytes;
  struct lazy_raw* p_lazy_counts;
  struct group_infos* p_group_infos;
  struct lazy_chr* p_lazy_x_reencoded;
  struct truelength_info* p_truelength_info;
};

#define PROTECT_ORDER_INFO(p_order_info, p_n) do {                    \
  PROTECT((p_order_info)->self);                                      \
  *(p_n) += 1;                                                        \
  PROTECT_ORDER((p_order_info)->p_order, (p_n));                      \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_x_chunk, (p_n));            \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_x_aux, (p_n));              \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_o_aux, (p_n));              \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_bytes, (p_n));              \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_counts, (p_n));             \
  PROTECT_GROUP_INFOS((p_order_info)->p_group_infos, (p_n));          \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_x_reencoded, (p_n));        \
  PROTECT_TRUELENGTH_INFO((p_order_info)->p_truelength_info, (p_n));  \
} while (0)

static inline int order_info_n_groups(struct order_info* p_info) {
  return groups_current(p_info->p_group_infos)->n_groups;
}

static inline SEXP order_info_group_sizes(struct order_info* p_info) {
  return groups_current(p_info->p_group_infos)->data;
}
static inline int* order_info_p_group_sizes(struct order_info* p_info) {
  return groups_current(p_info->p_group_infos)->p_data;
}
static inline SEXP order_info_group_sizes_values(struct order_info* p_info) {
  return int_resize(
    order_info_group_sizes(p_info),
    groups_current(p_info->p_group_infos)->data_size,
    order_info_n_groups(p_info)
  );
}

static inline SEXP order_info_o(struct order_info* p_info) {
  return p_info->p_order->data;
}
static inline int* order_info_p_o(struct order_info* p_info) {
  return p_info->p_order->p_data;
}

// -----------------------------------------------------------------------------

enum order_force_tracking {
  ORDER_FORCE_TRACKING_true,
  ORDER_FORCE_TRACKING_false
};

enum order_overwrite {
  ORDER_OVERWRITE_false,
  ORDER_OVERWRITE_true
};

/*
 * Order `x` and return information about the ordering
 *
 * `vec_order_info()` returns an `order_info` struct. Members of that struct
 * should be accessed with one of the `order_info_*()` utilities. If
 * `force == ORDER_FORCE_TRACKING_true`, then group sizes will be tracked as
 * well.
 *
 * The result should be protected with `PROTECT_ORDER_INFO()`.
 */
struct order_info* vec_order_info(SEXP x,
                                  SEXP decreasing,
                                  SEXP na_last,
                                  const enum order_force_tracking force);

/*
 * @member overwrite Should the order currently in `p_info->p_order` be
 *   overwritten? If not, a new ordering vector is allocated.
 *
 * @member o_unique Storage for the elements of the original `p_info->p_order`
 *   that align with the sorted unique values of `x`.
 *
 * @member p_info An `order_info` struct pointer from a previous call to
 *   `vec_order_info()`. Its `p_info->p_order` element will be updated, but
 *   the group information will be left untouched.
 *
 * `vec_order_uniques_by_appearance()` does the following:
 *
 * - The elements of `p_info->p_order` corresponding to sorted unique elements
 *   in `x` are extracted into `o_unique`.
 *
 * - `o_unique` is then ordered, and the corresponding ordering is placed into
 *   `p_info->p_order`. This second ordering allows you to construct an
 *   ordering of `x` by appearance.
 *
 * Notes:
 *
 * - The input `p_info` should be the result of a previous call to
 *   `vec_order_info()` with group tracking turned on.
 *
 * - Group tracking is turned off, so it is safe to assume that this doesn't
 *   modify the group sizes.
 *
 * - The returned `p_info` might contain a newly allocated `p_info->p_order`
 *   object, so `PROTECT_ORDER_INFO()` must be re-called on it.
 */
struct order_info* vec_order_uniques_by_appearance(const enum order_overwrite overwrite,
                                                   SEXP o_unique,
                                                   struct order_info* p_info);

// -----------------------------------------------------------------------------
#endif
