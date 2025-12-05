#ifndef VCTRS_ASSERT_H
#define VCTRS_ASSERT_H

#include "vctrs-core.h"
#include "conditions.h"
#include "dim.h"
#include "size.h"
#include "utils-dispatch.h"

// ----------------------------------------------------------------------

/**
 * Whether or not `NULL` values are allowed
 */
enum vctrs_allow_null {
  VCTRS_ALLOW_NULL_no = 0,
  VCTRS_ALLOW_NULL_yes = 1
};

static inline
bool allow_null_as_bool(enum vctrs_allow_null allow_null) {
  return (bool) allow_null;
}

static inline
enum vctrs_allow_null arg_as_allow_null(r_obj* x, const char* arg) {
  return r_arg_as_bool(x, arg) ? VCTRS_ALLOW_NULL_yes : VCTRS_ALLOW_NULL_no;
}

// ----------------------------------------------------------------------
// Vector check

static inline
bool obj_is_vector(r_obj* x, enum vctrs_allow_null allow_null) {
  if (x == r_null) {
    return allow_null_as_bool(allow_null);
  }
  struct vctrs_proxy_info info = vec_proxy_info(x);
  return info.type != VCTRS_TYPE_scalar;
}

static inline
void obj_check_vector(
  r_obj* x,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
) {
  if (!obj_is_vector(x, allow_null)) {
    stop_scalar_type(x, p_x_arg, call);
  }
}

bool list_all_vectors(
  r_obj* xs,
  enum vctrs_allow_null allow_null
);

void list_check_all_vectors(
  r_obj* xs,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

// ----------------------------------------------------------------------
// Size check

static inline
bool vec_is_size(
  r_obj* x,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
) {
  const r_ssize x_size = vec_size_params(x, p_x_arg, call);

  if (x_size == size) {
    return true;
  }

  if (allow_null_as_bool(allow_null) && x == r_null) {
    return true;
  }

  return false;
}

static inline
void vec_check_size(
  r_obj* x,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
) {
  if (!vec_is_size(x, size, allow_null, p_x_arg, call)) {
    const r_ssize x_size = vec_size_params(x, p_x_arg, call);
    stop_assert_size(x_size, size, p_x_arg, call);
  }
}

bool list_all_size(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

void list_check_all_size(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

// ----------------------------------------------------------------------
// List check

r_no_return
void stop_non_list_type(
  r_obj* x,
  struct vctrs_arg* arg,
  struct r_lazy call
);

static inline
bool obj_is_list(r_obj* x) {
  // Require `x` to be a list internally
  if (r_typeof(x) != R_TYPE_list) {
    return false;
  }

  // List arrays are not lists for vctrs purposes. We have pretty deep
  // assumptions that if an object is a list, then `r_length(x) == vec_size(x)`.
  // See `list_drop_empty()` and `list_combine()` for examples of
  // implementations that would be broken if this wasn't true.
  if (has_dim(x)) {
    return false;
  }

  // Unclassed R_TYPE_list are lists
  if (!r_is_object(x)) {
    return true;
  }

  const enum vctrs_class_type type = class_type(x);

  // Classed R_TYPE_list are only lists if the last class is explicitly `"list"`
  // or if it is a bare "AsIs" type
  return (type == VCTRS_CLASS_list) || (type == VCTRS_CLASS_bare_asis);
}

static inline
void obj_check_list(
  r_obj* x,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
) {
  if (!obj_is_list(x)) {
    stop_non_list_type(x, p_x_arg, call);
  }
}

// ----------------------------------------------------------------------
// Recyclable check

static inline
bool vec_is_recyclable(
  r_obj* x,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
) {
  const r_ssize x_size = vec_size_params(x, p_x_arg, call);

  if (x_size == size || x_size == 1) {
    return true;
  }

  if (allow_null_as_bool(allow_null) && x == r_null) {
    return true;
  }

  return false;
}

static inline
r_ssize vec_check_recyclable(
  r_obj* x,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_x_arg,
  struct r_lazy call
) {
  const r_ssize x_size = vec_size_params(x, p_x_arg, call);

  if (x_size == size || x_size == 1) {
    return x_size;
  }

  // It is up to the caller to be prepared to handle this!
  if (allow_null_as_bool(allow_null) && x == r_null) {
    return 0;
  }

  stop_recycle_incompatible_size(x_size, size, p_x_arg, call);
}

bool list_all_recyclable(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

void list_check_all_recyclable(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

#endif
