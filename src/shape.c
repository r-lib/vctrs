#include "vctrs.h"
#include "decl/shape-decl.h"

// -----------------------------------------------------------------------------

r_obj* ffi_vec_shaped_ptype(r_obj* ffi_ptype, r_obj* ffi_x) {
  return vec_shaped_ptype(ffi_ptype, ffi_x);
}

r_obj* vec_shaped_ptype(r_obj* ptype, r_obj* x) {
  if (!has_dim(x)) {
    // By far the most common case
    return ptype;
  }

  r_obj* x_dimensions = r_dim(x);
  r_obj* x_shape = KEEP(dims_shape(x_dimensions));

  ptype = KEEP(r_clone_referenced(ptype));
  r_attrib_poke_dim(ptype, x_shape);

  FREE(2);
  return ptype;
}

// -----------------------------------------------------------------------------

r_obj* ffi_vec_shaped_ptype2(r_obj* ptype, r_obj* x, r_obj* y, r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  return vec_shaped_ptype2(ptype, x, y, &x_arg, &y_arg);
}

r_obj* vec_shaped_ptype2(
  r_obj* ptype,
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
) {
  r_obj* shape = vec_shape2(x, y, p_x_arg, p_y_arg);

  if (shape == r_null) {
    return ptype;
  }

  // Only `KEEP()` if we have to
  KEEP(shape);

  ptype = KEEP(r_clone_referenced(ptype));
  r_attrib_poke_dim(ptype, shape);

  FREE(2);
  return ptype;
}

// -----------------------------------------------------------------------------

r_obj* ffi_vec_shape2(r_obj* x, r_obj* y, r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  return vec_shape2(x, y, &x_arg, &y_arg);
}

static inline
r_obj* vec_shape2(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
) {
  // Expect that `r_dim()` does not allocate, so we don't protect these!
  // This is somewhat important for performance, because `vec_shaped_ptype2()`
  // is called on every ptype2 iteration.
  r_obj* x_dimensions = r_dim(x);
  r_obj* y_dimensions = r_dim(y);

  if (x_dimensions == r_null) {
    if (y_dimensions == r_null) {
      return r_null;
    } else {
      return dims_shape(y_dimensions);
    }
  } else {
    if (y_dimensions == r_null) {
      return dims_shape(x_dimensions);
    } else {
      return dims_shape2(x_dimensions, y_dimensions, x, y, p_x_arg, p_y_arg);
    }
  }
}

// -----------------------------------------------------------------------------

// Sets the first axis to zero
static inline
r_obj* dims_shape(r_obj* dimensions) {
  if (r_length(dimensions) == 0) {
    r_stop_internal("`dimensions` must have length.");
  }
  if (r_typeof(dimensions) != R_TYPE_integer) {
    r_stop_internal("`dimensions` must be an integer vector.");
  }

  if (r_int_get(dimensions, 0) == 0) {
    // Already a shape, no clone required
    return dimensions;
  }

  dimensions = KEEP(r_clone_referenced(dimensions));
  r_int_begin(dimensions)[0] = 0;
  FREE(1);
  return dimensions;
}

// -----------------------------------------------------------------------------

/*
 * Returns a dimensions vector where the first dimension length is forcibly set
 * to 0, and the rest are the common shape of `x` and `y`.
 */
static inline
r_obj* dims_shape2(
  r_obj* x_dimensions,
  r_obj* y_dimensions,
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
) {
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

    p_out[i] = dim2(x_dimension, y_dimension, axis, x, y, p_x_arg, p_y_arg);
  }

  for (; i < max_dimensionality; ++i) {
    p_out[i] = p_max_dimensions[i];
  }

  FREE(1);
  return out;
}

static inline
int dim2(
  int x_dimension,
  int y_dimension,
  int axis,
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
) {
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

// -----------------------------------------------------------------------------

r_obj* vec_shape_broadcast(
  r_obj* x,
  r_obj* to,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_to_arg,
  struct r_lazy call
) {
  r_obj* ffi_x_arg = KEEP(vctrs_arg(p_x_arg));
  r_obj* ffi_to_arg = KEEP(vctrs_arg(p_to_arg));
  r_obj* ffi_call = KEEP(r_lazy_eval(call));

  r_obj* out = vctrs_eval_mask5(
    r_sym("shape_broadcast"),
    r_syms.x,
    x,
    r_sym("to"),
    to,
    syms.x_arg,
    ffi_x_arg,
    syms.to_arg,
    ffi_to_arg,
    r_syms.call,
    ffi_call
  );

  FREE(3);
  return out;
}
