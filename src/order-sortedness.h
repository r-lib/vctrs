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
                                     const char** p_x_strings,
                                     const bool* p_x_string_nas,
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

/*
 * Compare `x` to `y` lexicographically in a C-locale
 *
 * - `direction` is `1` for ascending and `-1` for descending
 * - `na_order` is `1` if `na_last = true` and `-1` if `na_last = false`
 */
 static inline
 int str_cmp(
  const char* x,
  const char* y,
  const bool x_string_na,
  const bool y_string_na,
  const int direction,
  const int na_order
) {
  // Same pointer - including `NA`s
  if (x == y) {
    return 0;
  }

  if (x_string_na) {
    return na_order;
  }

  if (y_string_na) {
    return -na_order;
  }

  return direction * strcmp(x, y);
}

/*
 * Compare `x` to `y` lexicographically in a C-locale with `pass` information
 *
 * The `pass` tells us that we know everything up to `pass` is already the same,
 * so we can avoid checking those in `strcmp()` if we get that far.
 */
static inline
int str_cmp_with_pass(
  const char* x,
  const char* y,
  const bool x_string_na,
  const bool y_string_na,
  const int x_string_size,
  const int direction,
  const int na_order,
  const int pass
) {
  // Same pointer - including `NA`s
  if (x == y) {
    return 0;
  }

  if (x_string_na) {
    return na_order;
  }

  if (y_string_na) {
    return -na_order;
  }

  if (pass == 0) {
    // We don't know anything yet
    return direction * strcmp(x, y);
  }

  // Otherwise we know they are equal up to the position before `pass`, but
  // it might have been equality with implicit "" so we need to check the
  // length of one of them
  const int last_pass = pass - 1;

  // We are comparing length with C 0-based indexing so we have to do +1.
  if (x_string_size < last_pass + 1) {
    // `y` is longer, so `x` must come first
    return 1;
  }

  // Now start the comparison at `last_pass`, which we know exists
  const char* x_starting_from_last_pass = x + last_pass;
  const char* y_starting_from_last_pass = y + last_pass;

  return direction * strcmp(x_starting_from_last_pass, y_starting_from_last_pass);
}

// -----------------------------------------------------------------------------
#endif
