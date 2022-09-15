#include "vctrs.h"
#include "decl/shape-decl.h"


// Computes the common shape of `x` and `y` and attaches it as the
// dimensions of `ptype`. If `x` and `y` are both atomic with `NULL` dimensions,
// then no dimensions are attached and `ptype` is returned unmodified.
// [[ include("shape.h") ]]
r_obj* vec_shaped_ptype(r_obj* ptype,
                        r_obj* x, r_obj* y,
                        struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg) {
  r_obj* ptype_dimensions = KEEP(vec_shape2(x, y, p_x_arg, p_y_arg));

  if (ptype_dimensions == r_null) {
    FREE(1);
    return ptype;
  }

  ptype = KEEP(r_clone_referenced(ptype));
  r_attrib_poke_dim(ptype, ptype_dimensions);

  FREE(2);
  return ptype;
}

r_obj* ffi_vec_shaped_ptype(r_obj* ptype, r_obj* x, r_obj* y, r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  return vec_shaped_ptype(ptype, x, y, &x_arg, &y_arg);
}


// -----------------------------------------------------------------------------

static
r_obj* vec_shape2(r_obj* x,
                  r_obj* y,
                  struct vctrs_arg* p_x_arg,
                  struct vctrs_arg* p_y_arg) {
  r_obj* x_dimensions = KEEP(r_dim(x));
  r_obj* y_dimensions = KEEP(r_dim(y));

  r_obj* out = vec_shape2_impl(x_dimensions, y_dimensions, x, y, p_x_arg, p_y_arg);

  FREE(2);
  return out;
}

r_obj* ffi_vec_shape2(r_obj* x, r_obj* y, r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  return vec_shape2(x, y, &x_arg, &y_arg);
}


/*
 * Returns `NULL` if `x` and `y` are atomic.
 * Otherwise returns a dimensions vector where the first dimension length
 * is forcibly set to 0, and the rest are the common shape of `x` and `y`.
 */
static
r_obj* vec_shape2_impl(r_obj* x_dimensions,
                       r_obj* y_dimensions,
                       r_obj* x,
                       r_obj* y,
                       struct vctrs_arg* p_x_arg,
                       struct vctrs_arg* p_y_arg) {
  if (x_dimensions == r_null) {
    return vec_shape(y_dimensions);
  }
  if (y_dimensions == r_null) {
    return vec_shape(x_dimensions);
  }

  r_ssize x_dimensionality = r_length(x_dimensions);
  r_ssize y_dimensionality = r_length(y_dimensions);

  r_obj* max_dimensions;
  r_ssize max_dimensionality;
  r_ssize min_dimensionality;

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
    r_stop_internal("`max_dimensionality` must have length.");
  }

  const int* p_x_dimensions = r_int_cbegin(x_dimensions);
  const int* p_y_dimensions = r_int_cbegin(y_dimensions);
  const int* p_max_dimensions = r_int_cbegin(max_dimensions);

  r_obj* out = KEEP(r_alloc_integer(max_dimensionality));
  int* p_out = r_int_begin(out);

  // Set the first axis to zero
  p_out[0] = 0;

  // Start loop at the second axis
  r_ssize i = 1;

  for (; i < min_dimensionality; ++i) {
    const int axis = i + 1;
    const int x_dimension = p_x_dimensions[i];
    const int y_dimension = p_y_dimensions[i];

    p_out[i] = vec_dimension2(x_dimension, y_dimension, axis, x, y, p_x_arg, p_y_arg);
  }

  for (; i < max_dimensionality; ++i) {
    p_out[i] = p_max_dimensions[i];
  }

  FREE(1);
  return out;
}


// -----------------------------------------------------------------------------

// Sets the first axis to zero
static
r_obj* vec_shape(r_obj* dimensions) {
  if (dimensions == r_null) {
    return r_null;
  }

  dimensions = KEEP(r_clone_referenced(dimensions));

  if (r_length(dimensions) == 0) {
    r_stop_internal("`dimensions` must have length.");
  }

  if (r_typeof(dimensions) != R_TYPE_integer) {
    r_stop_internal("`dimensions` must be an integer vector.");
  }

  r_int_begin(dimensions)[0] = 0;

  FREE(1);
  return dimensions;
}

static inline
int vec_dimension2(int x_dimension,
                   int y_dimension,
                   int axis,
                   r_obj* x,
                   r_obj* y,
                   struct vctrs_arg* p_x_arg,
                   struct vctrs_arg* p_y_arg) {
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
