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

#ifndef VCTRS_ORDER_TRANSFORM_H
#define VCTRS_ORDER_TRANSFORM_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

/*
 * `proxy_chr_transform()` iterates over `proxy`, applying `chr_transform`
 * on any character vectors that it detects.
 *
 * It expects that:
 * - If `proxy` is a data frame, it has been flattened by its corresponding
 *   `vec_proxy_*()` function.
 * - All character vectors in `proxy` have already been normalized to UTF-8
 *   by `vec_normalize_encoding()`.
 */
SEXP proxy_chr_transform(SEXP proxy, SEXP chr_transform);

// -----------------------------------------------------------------------------
#endif
