#include "shape.h"
#include "size.h"

// [[ register() ]]
SEXP vctrs_shaped_ptype(SEXP ptype, SEXP x, SEXP y) {
  return vec_shaped_ptype(ptype, x, y);
}

static SEXP vec_shape2(SEXP x_dimensions, SEXP y_dimensions);

// Computes the common shape of `x` and `y` and attaches it as the
// dimensions of `ptype`. If `x` and `y` are both atomic with `NULL` dimensions,
// then no dimensions are attached and `ptype` is returned unmodified.
// [[ include("shape.h") ]]
SEXP vec_shaped_ptype(SEXP ptype, SEXP x, SEXP y) {
  SEXP x_dimensions = PROTECT(r_dim(x));
  SEXP y_dimensions = PROTECT(r_dim(y));

  SEXP ptype_dimensions = PROTECT(vec_shape2(x_dimensions, y_dimensions));

  if (ptype_dimensions == R_NilValue) {
    UNPROTECT(3);
    return ptype;
  }

  ptype = PROTECT(r_maybe_duplicate(ptype));

  r_poke_dim(ptype, ptype_dimensions);

  UNPROTECT(4);
  return ptype;
}

// -----------------------------------------------------------------------------

static SEXP vec_shape2(SEXP x_dimensions, SEXP y_dimensions);

// [[ register() ]]
SEXP vctrs_shape2(SEXP x_dimensions, SEXP y_dimensions) {
  return vec_shape2(x_dimensions, y_dimensions);
}

static inline SEXP vec_shape(SEXP dimensions);
static inline int vec_dimension2(int axis, int x_dimension, int y_dimension);

/*
 * Returns `NULL` if `x` and `y` are atomic.
 * Otherwise returns a dimensions vector where the first dimension length
 * is forcibly set to 0, and the rest are the common shape of `x` and `y`.
 */
static SEXP vec_shape2(SEXP x_dimensions, SEXP y_dimensions) {
  if (x_dimensions == R_NilValue) {
    return vec_shape(y_dimensions);
  }
  if (y_dimensions == R_NilValue) {
    return vec_shape(x_dimensions);
  }

  R_len_t x_dimensionality = Rf_length(x_dimensions);
  R_len_t y_dimensionality = Rf_length(y_dimensions);

  SEXP max_dimensions;
  R_len_t max_dimensionality;
  R_len_t min_dimensionality;

  if (x_dimensionality >= y_dimensionality) {
    max_dimensions = x_dimensions;
    max_dimensionality = x_dimensionality;
    min_dimensionality = y_dimensionality;
  } else {
    max_dimensions = y_dimensions;
    max_dimensionality = y_dimensionality;
    min_dimensionality = x_dimensionality;
  }

  // Sanity check, should never be true
  if (max_dimensionality == 0) {
    Rf_errorcall(R_NilValue, "Internal error: `max_dimensionality` must have length.");
  }

  const int* p_x_dimensions = INTEGER_RO(x_dimensions);
  const int* p_y_dimensions = INTEGER_RO(y_dimensions);
  const int* p_max_dimensions = INTEGER_RO(max_dimensions);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, max_dimensionality));
  int* p_out = INTEGER(out);

  // Set the first axis to zero
  p_out[0] = 0;

  // Start loop at the second axis
  R_len_t i = 1;

  for (; i < min_dimensionality; ++i) {
    p_out[i] = vec_dimension2(i + 1, p_x_dimensions[i], p_y_dimensions[i]);
  }

  for (; i < max_dimensionality; ++i) {
    p_out[i] = p_max_dimensions[i];
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// Sets the first axis to zero
static inline SEXP vec_shape(SEXP dimensions) {
  if (dimensions == R_NilValue) {
    return R_NilValue;
  }

  dimensions = PROTECT(r_maybe_duplicate(dimensions));

  if (Rf_length(dimensions) == 0) {
    Rf_errorcall(R_NilValue, "Internal error: `dimensions` must have length.");
  }

  if (TYPEOF(dimensions) != INTSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `dimensions` must be an integer vector.");
  }

  INTEGER(dimensions)[0] = 0;

  UNPROTECT(1);
  return dimensions;
}

static inline int vec_dimension2(int axis, int x_dimension, int y_dimension) {
  if (x_dimension == y_dimension) {
    return x_dimension;
  } else if (x_dimension == 1) {
    return y_dimension;
  } else if (y_dimension == 1) {
    return x_dimension;
  } else {
    Rf_errorcall(R_NilValue, "Incompatible lengths along axis %i: %i, %i.", axis, x_dimension, y_dimension);
  }
}
