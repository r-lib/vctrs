#ifndef VCTRS_ORDER_LAZY_H
#define VCTRS_ORDER_LAZY_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

/*
 * `lazy_vec` is a lazy raw vector that only allocates itself when
 * `lazy_vec_initialize()` is called. It is used as working memory of varying
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
 * @member size The total size of the RAWSXP to allocate. This is generally
 *   computed as `size * multiplier` in `new_lazy_vec()`, where multiplier
 *   is a `sizeof(<type>)`.
 * @member initialized Has the lazy vector been initialized yet?
 */
struct lazy_vec {
  SEXP data;
  void* p_data;
  PROTECT_INDEX data_pi;

  R_xlen_t size;
  bool initialized;
};

#define PROTECT_LAZY_VEC(p_info, p_n) do {                     \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi);      \
  *(p_n) += 1;                                                 \
} while (0)


/*
 * Construct a new lazy vector
 *
 * Pair with `PROTECT_LAZY_VEC()`.
 *
 * @param size The size of the type you want to interpret the memory as.
 * @param multiplier A `sizeof(<type>)` result for the type you are allocating
 *   memory for.
 */
static inline
struct lazy_vec new_lazy_vec(R_xlen_t size, size_t multiplier) {
  return (struct lazy_vec) {
    .data = R_NilValue,
    .size = size * multiplier,
    .initialized = false
  };
}

/*
 * Allocate the lazy vector if it hasn't already been allocated.
 * This reprotects itself using the protection index.
 */
static inline
void lazy_vec_initialize(struct lazy_vec* p_x) {
  if (p_x->initialized) {
    return;
  }

  p_x->data = Rf_allocVector(RAWSXP, p_x->size);

  REPROTECT(p_x->data, p_x->data_pi);

  p_x->p_data = (void*) RAW(p_x->data);

  p_x->initialized = true;
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

  R_xlen_t size;
  bool initialized;
};

#define PROTECT_LAZY_INT(p_info, p_n) do { \
  PROTECT((p_info)->data);                 \
  *(p_n) += 1;                             \
} while (0)


static inline
struct lazy_int new_lazy_int(R_xlen_t size) {
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
void lazy_order_initialize(struct lazy_int* p_lazy_o) {
  if (p_lazy_o->initialized) {
    return;
  }

  R_xlen_t size = p_lazy_o->size;

  int* p_data = p_lazy_o->p_data;

  // Initialize `x` with sequential 1-based ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    p_data[i] = i + 1;
  }

  p_lazy_o->initialized = true;
}

// -----------------------------------------------------------------------------
#endif
