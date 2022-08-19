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

#include "vctrs.h"

// -----------------------------------------------------------------------------

static SEXP chr_apply(SEXP x, SEXP chr_proxy_collate);
static SEXP df_apply(SEXP x, SEXP chr_proxy_collate);

// [[ include("order-collate.h") ]]
SEXP proxy_apply_chr_proxy_collate(SEXP proxy, SEXP chr_proxy_collate) {
  if (chr_proxy_collate == r_null) {
    return proxy;
  }

  chr_proxy_collate = PROTECT(r_as_function(chr_proxy_collate, "chr_proxy_collate"));

  SEXP out;

  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_character: out = chr_apply(proxy, chr_proxy_collate); break;
  case vctrs_type_dataframe: out = df_apply(proxy, chr_proxy_collate); break;
  default: out = proxy;
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static
SEXP chr_apply(SEXP x, SEXP chr_proxy_collate) {
  // Don't use vctrs dispatch utils because we match argument positionally
  SEXP call = PROTECT(Rf_lang2(syms_chr_proxy_collate, syms_x));

  SEXP mask = PROTECT(r_new_environment(R_GlobalEnv));
  Rf_defineVar(syms_chr_proxy_collate, chr_proxy_collate, mask);
  Rf_defineVar(syms_x, x, mask);

  SEXP out = PROTECT(Rf_eval(call, mask));

  if (vec_typeof(out) != vctrs_type_character) {
    Rf_errorcall(
      R_NilValue,
      "`chr_proxy_collate` must return a character vector."
    );
  }

  R_len_t x_size = vec_size(x);
  R_len_t out_size = vec_size(out);

  if (x_size != out_size) {
    Rf_errorcall(
      R_NilValue,
      "`chr_proxy_collate` must return a vector of the same length (%i, not %i).",
      x_size,
      out_size
    );
  }

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

static
SEXP df_apply(SEXP x, SEXP chr_proxy_collate) {
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

    col = chr_apply(col, chr_proxy_collate);
    SET_VECTOR_ELT(out, i, col);
  }

  UNPROTECT(1);
  return out;
}
