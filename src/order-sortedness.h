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

#ifndef VCTRS_ORDER_SORTEDNESS_H
#define VCTRS_ORDER_SORTEDNESS_H

#include "vctrs-core.h"
#include "order-groups.h"
#include "order-string.h"

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
                                     bool nan_distinct,
                                     struct group_infos* p_group_infos);

enum vctrs_sortedness int_sortedness(const int* p_x,
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
                                     struct group_infos* p_group_infos);

enum vctrs_sortedness chr_sortedness(const SEXP* p_x,
                                     const struct str_info* p_x_info,
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
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
