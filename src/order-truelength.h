#ifndef VCTRS_ORDER_TRUELENGTH_H
#define VCTRS_ORDER_TRUELENGTH_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

// This seems to be a reasonable default to start with for tracking the original
// truelengths of the unique strings, and is what base R uses. It is expanded
// by 2x every time we need to reallocate.
#define TRUELENGTH_DATA_SIZE_DEFAULT 10000

// -----------------------------------------------------------------------------

struct truelength_info {
  SEXP strings;
  SEXP* p_strings;
  PROTECT_INDEX strings_pi;

  SEXP lengths;
  R_xlen_t* p_lengths;
  PROTECT_INDEX lengths_pi;

  SEXP uniques;
  SEXP* p_uniques;
  PROTECT_INDEX uniques_pi;

  SEXP sizes;
  int* p_sizes;
  PROTECT_INDEX sizes_pi;

  SEXP sizes_aux;
  int* p_sizes_aux;
  PROTECT_INDEX sizes_aux_pi;

  R_xlen_t size_alloc;
  R_xlen_t size_used;
};

#define PROTECT_TRUELENGTH_INFO(p_info, p_n) do {                   \
  PROTECT_WITH_INDEX((p_info)->strings, &(p_info)->strings_pi);     \
  (p_info)->p_strings = STRING_PTR((p_info)->strings);              \
                                                                    \
  PROTECT_WITH_INDEX((p_info)->lengths, &(p_info)->lengths_pi);     \
  (p_info)->p_lengths = (R_xlen_t*) RAW((p_info)->lengths);         \
                                                                    \
  PROTECT_WITH_INDEX((p_info)->uniques, &(p_info)->uniques_pi);     \
  (p_info)->p_uniques = STRING_PTR((p_info)->uniques);              \
                                                                    \
  PROTECT_WITH_INDEX((p_info)->sizes, &(p_info)->sizes_pi);         \
  (p_info)->p_sizes = INTEGER((p_info)->sizes);                     \
                                                                    \
  PROTECT_WITH_INDEX((p_info)->sizes_aux, &(p_info)->sizes_aux_pi); \
  (p_info)->p_sizes_aux = INTEGER((p_info)->sizes_aux);             \
                                                                    \
  *(p_n) += 5;                                                      \
} while(0)

// -----------------------------------------------------------------------------
#endif
