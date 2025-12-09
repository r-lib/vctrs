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

#ifndef VCTRS_ORDER_STRING_H
#define VCTRS_ORDER_STRING_H

#include "vctrs-core.h"

/*
 * Cached information for a single `STRSXP`. Faster than `CHAR()` and
 * `Rf_length()` since we access repeatedly, at the cost of more memory.
 */
struct str_info {
  SEXP x;
  const char* p_x;
  int size;
};

#endif
