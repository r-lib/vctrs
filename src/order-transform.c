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

#include "order-transform.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static SEXP chr_apply_transform(SEXP x, SEXP chr_transform);
static SEXP df_apply_transform(SEXP x, SEXP chr_transform);

// [[ include("order-transform.h") ]]
SEXP proxy_chr_transform(SEXP proxy, SEXP chr_transform) {
  if (chr_transform == r_null) {
    return proxy;
  }

  chr_transform = PROTECT(r_as_function(chr_transform, "chr_transform"));

  SEXP out;

  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_character: out = chr_apply_transform(proxy, chr_transform); break;
  case vctrs_type_dataframe: out = df_apply_transform(proxy, chr_transform); break;
  default: out = proxy;
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static
SEXP chr_apply_transform(SEXP x, SEXP chr_transform) {
  // Don't use vctrs dispatch utils because we match argument positionally
  SEXP call = PROTECT(Rf_lang2(syms_chr_transform, syms_x));

  SEXP mask = PROTECT(r_new_environment(R_GlobalEnv));
  Rf_defineVar(syms_chr_transform, chr_transform, mask);
  Rf_defineVar(syms_x, x, mask);

  SEXP out = PROTECT(Rf_eval(call, mask));

  if (vec_typeof(out) != vctrs_type_character) {
    Rf_errorcall(
      R_NilValue,
      "`chr_transform` must return a character vector."
    );
  }

  R_len_t x_size = vec_size(x);
  R_len_t out_size = vec_size(out);

  if (x_size != out_size) {
    Rf_errorcall(
      R_NilValue,
      "`chr_transform` must return a vector of the same length (%i, not %i).",
      x_size,
      out_size
    );
  }

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

static
SEXP df_apply_transform(SEXP x, SEXP chr_transform) {
  const r_ssize n_cols = r_length(x);
  const SEXP* v_x = VECTOR_PTR_RO(x);

  r_ssize i = 0;

  for (; i < n_cols; ++i) {
    SEXP col = v_x[i];
    if (vec_proxy_typeof(col) == vctrs_type_character) {
      break;
    }
  }

  if (i == n_cols) {
    // No character columns
    return x;
  }

  SEXP out = PROTECT(r_clone_referenced(x));

  for (; i < n_cols; ++i) {
    SEXP col = v_x[i];

    if (vec_proxy_typeof(col) != vctrs_type_character) {
      continue;
    }

    col = chr_apply_transform(col, chr_transform);
    SET_VECTOR_ELT(out, i, col);
  }

  UNPROTECT(1);
  return out;
}
