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
  SEXP x,
  SEXP y,
  const char* x_string,
  const char* y_string,
  const int direction,
  const int na_order
) {
  // Same pointer - including `NA`s
  if (x == y) {
    return 0;
  }

  if (x == NA_STRING) {
    return na_order;
  }

  if (y == NA_STRING) {
    return -na_order;
  }

  return direction * strcmp(x_string, y_string);
}

/*
 * Compare `x` to `y` lexicographically in a C-locale with `pass` information
 *
 * The `pass` tells us that we know everything up to `pass` is already the same,
 * so we can avoid checking those in `strcmp()` if we get that far.
 *
 * `pass` will always advance `x` and `y` up to but never past the nul terminator
 * of each respective string, i.e. `\0`. It is actually well-defined for us to
 * compare against the nul terminator, it is identical to `0`, which means that
 * the shorter string will compare as "less than" the longer string, which is
 * correct! For example, for `"ab\0"` and `"abc\0" it is possible to have
 * `pass == 2`, which means that we have already compared `a` and `b` and know
 * they are equivalent. This advances us to `"\0"` and `"c\0"`, where `"\0"` then
 * compares as less than `"c\0"`, which is correct!
 *
 * Guaranteed to not be `NA`s
 */
static inline
int str_cmp_with_pass(
  const char* x,
  const char* y,
  const int direction,
  const int pass
) {
  // Same pointer
  // In our research it seems like `strcmp()` doesn't optimize this check,
  // since it would be rare for most `strcmp()` usage. But for R's interned
  // strings it definitely matters for us.
  if (x == y) {
    return 0;
  }

  // Start the comparison at `pass`, because we have checked everything before it
  const char* x_starting_from_last_pass = x + pass;
  const char* y_starting_from_last_pass = y + pass;

  return direction * strcmp(x_starting_from_last_pass, y_starting_from_last_pass);
}

// -----------------------------------------------------------------------------
#endif
