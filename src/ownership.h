#ifndef VCTRS_OWNERSHIP_H
#define VCTRS_OWNERSHIP_H

#include "vctrs-core.h"
#include "utils.h"

// Wrapper around `r_clone_referenced()` that knows about `ownership`
//
// For example, if we have `VCTRS_OWNERSHIP_deep` ownership over a data frame
// and one of its columns passes through here, we won't clone it even though it
// has a refcount >0.
//
// For `VCTRS_OWNERSHIP_foreign`, we only clone if not already referenced. The
// goal is generally to avoid cloning temporary values passed to vctrs
// functions.
static inline SEXP vec_clone_referenced(SEXP x, const enum vctrs_ownership ownership) {
  switch (ownership) {
  case VCTRS_OWNERSHIP_foreign: return r_clone_referenced(x);
  case VCTRS_OWNERSHIP_shallow: return x;
  case VCTRS_OWNERSHIP_deep: return x;
  default: r_stop_unreachable();
  }
}

#endif
