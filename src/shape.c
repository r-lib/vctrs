#include <rlang.h>
#include "shape.h"
#include "dim.h"

// [[ register() ]]
SEXP vctrs_shaped_ptype(SEXP ptype, SEXP x, SEXP y, SEXP frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame};
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame};
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  return vec_shaped_ptype(ptype, x, y, &x_arg, &y_arg);
}

static SEXP vec_shape2(SEXP x, SEXP y, struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);

// Computes the common shape of `x` and `y` and attaches it as the
// dimensions of `ptype`. If `x` and `y` are both atomic with `NULL` dimensions,
// then no dimensions are attached and `ptype` is returned unmodified.
// [[ include("shape.h") ]]
SEXP vec_shaped_ptype(SEXP ptype,
                      SEXP x, SEXP y,
                      struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg) {
  SEXP ptype_dimensions = PROTECT(vec_shape2(x, y, p_x_arg, p_y_arg));

  if (ptype_dimensions == R_NilValue) {
    UNPROTECT(1);
    return ptype;
  }

  ptype = PROTECT(r_clone_referenced(ptype));

  r_attrib_poke_dim(ptype, ptype_dimensions);

  UNPROTECT(2);
  return ptype;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_shape2(SEXP x, SEXP y, SEXP frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame};
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame};
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  return vec_shape2(x, y, &x_arg, &y_arg);
}

static SEXP vec_shape2_impl(SEXP x_dimensions, SEXP y_dimensions,
                            SEXP x, SEXP y,
                            struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);

static SEXP vec_shape2(SEXP x, SEXP y, struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg) {
  SEXP x_dimensions = PROTECT(r_dim(x));
  SEXP y_dimensions = PROTECT(r_dim(y));

  SEXP out = vec_shape2_impl(x_dimensions, y_dimensions, x, y, p_x_arg, p_y_arg);

  UNPROTECT(2);
  return out;
}

static SEXP vec_shape(SEXP dimensions);
static inline int vec_dimension2(int x_dimension, int y_dimension,
                                 int axis,
                                 SEXP x, SEXP y,
                                 struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);

/*
 * Returns `NULL` if `x` and `y` are atomic.
 * Otherwise returns a dimensions vector where the first dimension length
 * is forcibly set to 0, and the rest are the common shape of `x` and `y`.
 */
static SEXP vec_shape2_impl(SEXP x_dimensions, SEXP y_dimensions,
                            SEXP x, SEXP y,
                            struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg) {
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
    r_stop_internal("vec_shape2_impl", "`max_dimensionality` must have length.");
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
    const int axis = i + 1;
    const int x_dimension = p_x_dimensions[i];
    const int y_dimension = p_y_dimensions[i];

    p_out[i] = vec_dimension2(x_dimension, y_dimension, axis, x, y, p_x_arg, p_y_arg);
  }

  for (; i < max_dimensionality; ++i) {
    p_out[i] = p_max_dimensions[i];
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// Sets the first axis to zero
static SEXP vec_shape(SEXP dimensions) {
  if (dimensions == R_NilValue) {
    return R_NilValue;
  }

  dimensions = PROTECT(r_clone_referenced(dimensions));

  if (Rf_length(dimensions) == 0) {
    r_stop_internal("vec_shape", "`dimensions` must have length.");
  }

  if (TYPEOF(dimensions) != INTSXP) {
    r_stop_internal("vec_shape", "`dimensions` must be an integer vector.");
  }

  INTEGER(dimensions)[0] = 0;

  UNPROTECT(1);
  return dimensions;
}

static inline int vec_dimension2(int x_dimension, int y_dimension,
                                 int axis,
                                 SEXP x, SEXP y,
                                 struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg) {
  if (x_dimension == y_dimension) {
    return x_dimension;
  } else if (x_dimension == 1) {
    return y_dimension;
  } else if (y_dimension == 1) {
    return x_dimension;
  } else {
    stop_incompatible_shape(x, y, x_dimension, y_dimension, axis, p_x_arg, p_y_arg);
  }
}
