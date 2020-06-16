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

// -----------------------------------------------------------------------------
#endif
