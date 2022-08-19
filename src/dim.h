#ifndef VCTRS_DIM_H
#define VCTRS_DIM_H

#include "vctrs-core.h"
#include "utils.h"

// These versions return NULL and 0 for bare vectors.
// This is useful to distinguish them from 1D arrays.
static inline SEXP vec_bare_dim(SEXP x) {
  return r_dim(x);
}
static inline R_len_t vec_bare_dim_n(SEXP x) {
  return Rf_length(vec_bare_dim(x));
}


static inline SEXP vec_dim(SEXP x) {
  SEXP dim = vec_bare_dim(x);

  if (dim == R_NilValue) {
    dim = r_int(Rf_length(x));
  }

  return dim;
}

static inline R_len_t vec_dim_n(SEXP x) {
  SEXP dim = vec_bare_dim(x);

  if (dim == R_NilValue) {
    return 1;
  }

  return Rf_length(dim);
}


static inline bool has_dim(SEXP x) {
  return ATTRIB(x) != R_NilValue && r_dim(x) != R_NilValue;
}

#endif
