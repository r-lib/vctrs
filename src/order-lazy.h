#ifndef VCTRS_ORDER_LAZY_H
#define VCTRS_ORDER_LAZY_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

struct lazy_vec {
  SEXP data;
  void* p_data;
  PROTECT_INDEX data_pi;

  R_xlen_t size;
  bool initialized;
};

#define PROTECT_LAZY_VEC(p_info, p_n) do {                     \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi);      \
  (p_info)->p_data = DATAPTR((p_info)->data);                  \
                                                               \
  *(p_n) += 1;                                                 \
} while (0)


// Pair with `PROTECT_LAZY_VEC()`
static inline struct lazy_vec new_lazy_vec(R_xlen_t size, size_t multiplier) {
  struct lazy_vec out;

  out.data = vctrs_shared_empty_raw;

  out.size = size * multiplier;
  out.initialized = false;

  return out;
}

static inline void lazy_vec_initialize(struct lazy_vec* p_x) {
  if (p_x->initialized) {
    return;
  }

  p_x->data = Rf_allocVector(RAWSXP, p_x->size);

  REPROTECT(p_x->data, p_x->data_pi);

  p_x->p_data = DATAPTR(p_x->data);

  p_x->initialized = true;
}

// -----------------------------------------------------------------------------
#endif
