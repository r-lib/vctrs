#ifndef VCTRS_ORDER_SORTEDNESS_H
#define VCTRS_ORDER_SORTEDNESS_H

#include "vctrs.h"
#include "order-groups.h"

// -----------------------------------------------------------------------------

enum vctrs_sortedness {
  VCTRS_SORTEDNESS_unsorted,
  VCTRS_SORTEDNESS_sorted,
  VCTRS_SORTEDNESS_reversed,
};

// -----------------------------------------------------------------------------

enum vctrs_sortedness dbl_sortedness(const double* p_x,
                                     struct group_infos* p_group_infos,
                                     R_xlen_t size,
                                     bool decreasing,
                                     bool na_last);

enum vctrs_sortedness int_sortedness(const int* p_x,
                                     struct group_infos* p_group_infos,
                                     R_xlen_t size,
                                     bool decreasing,
                                     bool na_last);

enum vctrs_sortedness chr_sortedness(const SEXP* p_x,
                                     struct group_infos* p_group_infos,
                                     R_xlen_t size,
                                     bool decreasing,
                                     bool na_last,
                                     bool check_encoding);

// -----------------------------------------------------------------------------

void ord_resolve_sortedness(int* p_o,
                            enum vctrs_sortedness sortedness,
                            R_xlen_t size);

void ord_resolve_sortedness_chunk(int* p_o,
                                  enum vctrs_sortedness sortedness,
                                  R_xlen_t size);

// -----------------------------------------------------------------------------
#endif
