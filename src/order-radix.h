/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2020, Data table team
 *
 * Alternatively, the contents of this file may be used under the terms
 * of the GNU General Public License Version 3, as described in the
 * package LICENSE.md.
 *
 * Copyright (c) 2020, RStudio
 *
 * The implementation of `vec_order()` was heavily inspired by the
 * implementation of radix sort in data.table and by their contribution
 * to R's `order()`. See LICENSE.note for more information.
 */

#ifndef VCTRS_ORDER_RADIX_H
#define VCTRS_ORDER_RADIX_H

#include "vctrs.h"

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
  SEXP data;
  int* p_data;
  r_ssize size;
  bool initialized;
};

#define PROTECT_ORDER(p_order, p_n) do { \
  PROTECT((p_order)->data);              \
  *(p_n) += 1;                           \
} while (0)

static inline
struct order new_order(r_ssize size) {
  SEXP data = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_data = INTEGER(data);

  struct order out = {
    .data = data,
    .p_data = p_data,
    .size = size,
    .initialized = false
  };

  UNPROTECT(1);
  return out;
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
#endif
