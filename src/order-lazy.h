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

#ifndef VCTRS_ORDER_LAZY_H
#define VCTRS_ORDER_LAZY_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

/*
 * `lazy_int` is a lazy integer vector intended to hold the ordering vector
 * in `vec_order()`. It is lazy not in the allocation, but in the initialization
 * of values. Typically it is initialized to a 1-based sequential ordering
 * which is rearranged by the internal algorithm. However, for the counting
 * order, the initialization is not required for the first integer column,
 * which can result in a nice performance improvement.
 */
struct lazy_int {
  SEXP data;
  int* p_data;

  r_ssize size;
  bool initialized;
};

#define PROTECT_LAZY_INT(p_info, p_n) do { \
  PROTECT((p_info)->data);                 \
  *(p_n) += 1;                             \
} while (0)


static inline
struct lazy_int new_lazy_int(r_ssize size) {
  SEXP data = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_data = INTEGER(data);

  struct lazy_int out = {
    .data = data,
    .p_data = p_data,
    .size = size,
    .initialized = false
  };

  UNPROTECT(1);
  return out;
}

static inline
int* lazy_order_initialize(struct lazy_int* p_lazy_o) {
  if (p_lazy_o->initialized) {
    return p_lazy_o->p_data;
  }

  r_ssize size = p_lazy_o->size;

  // Initialize `x` with sequential 1-based ordering
  for (r_ssize i = 0; i < size; ++i) {
    p_lazy_o->p_data[i] = i + 1;
  }

  p_lazy_o->initialized = true;

  return p_lazy_o->p_data;
}

// -----------------------------------------------------------------------------
#endif
