#include "assert.h"

// [[ include("assert.h") ]]
void vec_assert(r_obj* x, r_ssize size, struct vctrs_arg* arg) {
  vec_assert_vector(x, arg);

  if (size != -1) {
    // `size == -1` makes no assertion about size
    vec_assert_size(x, size, arg);
  }
}

// [[ include("assert.h") ]]
void vec_assert_vector(r_obj* x, struct vctrs_arg* arg) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg);
  }
}

// [[ include("assert.h") ]]
void vec_assert_size(r_obj* x, r_ssize size, struct vctrs_arg* arg) {
  r_ssize x_size = vec_size(x);

  if (x_size != size) {
    stop_assert_size(x_size, size, arg);
  }
}
