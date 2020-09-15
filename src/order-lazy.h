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
 * `lazy_raw` is a lazy raw vector that only allocates itself when
 * `lazy_raw_initialize()` is called. It is used as working memory of varying
 * types by `vec_order()`. In `vec_order()` we aren't always sure how much
 * working memory is required, but we want to reuse it where we can once we
 * do allocate it. These lazy vectors allow us to specify the maximum amount
 * required up front without actually allocating them, delaying that allocation
 * until they are truly needed.
 *
 * @member data The RAWSXP that gets allocated lazily.
 * @member p_data A void pointer to the RAWSXP.
 * @member data_pi A protection index to `data` so it can reprotect itself
 *   upon allocation.
 * @member n_bytes_data The total size of the RAWSXP to allocate.
 *   This is computed as `size * n_bytes` in `new_lazy_raw()`, where `n_bytes`
 *   is from `sizeof(<type>)`.
 * @member initialized Has the lazy vector been initialized yet?
 */
struct lazy_raw {
  SEXP data;
  void* p_data;
  PROTECT_INDEX data_pi;

  r_ssize n_bytes_data;
  bool initialized;
};

#define PROTECT_LAZY_RAW(p_info, p_n) do {                     \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi);      \
  *(p_n) += 1;                                                 \
} while (0)


/*
 * Construct a new lazy vector
 *
 * Pair with `PROTECT_LAZY_RAW()`.
 *
 * @param size The size of the type you want to interpret the memory as.
 * @param n_bytes A `sizeof(<type>)` result for the type you are allocating
 *   memory for.
 */
static inline
struct lazy_raw new_lazy_raw(r_ssize size, size_t n_bytes) {
  return (struct lazy_raw) {
    .data = R_NilValue,
    .n_bytes_data = size * n_bytes,
    .initialized = false
  };
}

/*
 * Allocate the lazy vector if it hasn't already been allocated.
 * This reprotects itself using the protection index.
 */
static inline
void* lazy_raw_initialize(struct lazy_raw* p_x) {
  if (p_x->initialized) {
    return p_x->p_data;
  }

  p_x->data = Rf_allocVector(RAWSXP, p_x->n_bytes_data);

  REPROTECT(p_x->data, p_x->data_pi);

  p_x->p_data = (void*) RAW(p_x->data);

  p_x->initialized = true;

  return p_x->p_data;
}

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

struct lazy_chr {
  SEXP data;
  const SEXP* p_data;
  PROTECT_INDEX data_pi;

  r_ssize size;
  bool initialized;
};

#define PROTECT_LAZY_CHR(p_info, p_n) do {                \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi); \
  *(p_n) += 1;                                            \
} while (0)

static inline
struct lazy_chr new_lazy_chr(r_ssize size) {
  return (struct lazy_chr) {
    .data = R_NilValue,
    .size = size,
    .initialized = false
  };
}

static inline
const SEXP* lazy_chr_initialize(struct lazy_chr* p_x) {
  if (p_x->initialized) {
    return p_x->p_data;
  }

  p_x->data = Rf_allocVector(STRSXP, p_x->size);

  REPROTECT(p_x->data, p_x->data_pi);

  p_x->p_data = STRING_PTR_RO(p_x->data);

  p_x->initialized = true;

  return p_x->p_data;
}

// -----------------------------------------------------------------------------
#endif
