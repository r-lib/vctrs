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
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
                                     struct group_infos* p_group_infos);

enum vctrs_sortedness int_sortedness(const int* p_x,
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
                                     struct group_infos* p_group_infos);

enum vctrs_sortedness chr_sortedness(const SEXP* p_x,
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
                                     bool check_encoding,
                                     struct group_infos* p_group_infos);

// -----------------------------------------------------------------------------

void ord_resolve_sortedness(enum vctrs_sortedness sortedness,
                            r_ssize size,
                            int* p_o);

void ord_resolve_sortedness_chunk(enum vctrs_sortedness sortedness,
                                  r_ssize size,
                                  int* p_o);

// -----------------------------------------------------------------------------
#endif
