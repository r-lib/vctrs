#ifndef VCTRS_OWNED_H
#define VCTRS_OWNED_H

#include "altrep.h"
#include "utils.h"


// Ownership is recursive
enum vctrs_owned {
  VCTRS_OWNED_false = 0,
  VCTRS_OWNED_true
};


static inline enum vctrs_owned vec_owned(SEXP x) {
  return NO_REFERENCES(x) ? VCTRS_OWNED_true : VCTRS_OWNED_false;
}

// Wrapper around `r_clone_referenced()` that only attempts to clone if
// we indicate that we don't own `x`, or if we do own `x` but it is ALTREP.
// If `x` is ALTREP, we must clone it before dereferencing, otherwise we get
// a pointer into the ALTREP internals rather than into the object it
// truly represents.
static inline SEXP vec_clone_referenced(SEXP x, const enum vctrs_owned owned) {
  if (owned == VCTRS_OWNED_false || ALTREP(x)) {
    return r_clone_referenced(x);
  } else {
    return x;
  }
}

#endif
