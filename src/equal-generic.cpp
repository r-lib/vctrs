#include "rlang.hpp"


static inline
int equal_scalar_na_equal(const int x, const int y) {
  return x == y;
}


template <enum r_type R_TYPE,
          typename C_TYPE = DEFAULT_CTYPE(R_TYPE)>
static
void equal_fill_na_equal(const void* p_x_payload,
                         const void* p_y_payload,
                         int (*eq)(const C_TYPE, const C_TYPE),
                         r_ssize size,
                         int* p_out) {
  const C_TYPE* p_x = (const C_TYPE*) p_x_payload;
  const C_TYPE* p_y = (const C_TYPE*) p_y_payload;

  for (r_ssize i = 0; i < size; ++i) {
    p_out[i] = eq(p_x[i], p_y[i]);
  }
}

static
void equal_fill_na_equal(enum r_type type,
                         const void* p_x,
                         const void* p_y,
                         r_ssize size,
                         int* p_out) {
  switch (type) {
  case r_type_logical: equal_fill_na_equal<r_type_logical>(p_x, p_y, &equal_scalar_na_equal, size, p_out); return;
  case r_type_integer: equal_fill_na_equal<r_type_integer>(p_x, p_y, &equal_scalar_na_equal, size, p_out); return;
  default: throw;
  }
}

extern "C" {

  void equal_fill(enum r_type type,
                  const void* p_x,
                  const void* p_y,
                  r_ssize size,
                  bool na_equal,
                  int* p_out,
                  int* err) {
    try {
      if (na_equal) {
        equal_fill_na_equal(type, p_x, p_y, size, p_out);
      } else {
        // equal_fill_na_propagate(type, p_x, p_y, size, p_out);
      }
    } catch (...) {
      *err = -1;
    }
  }

} // extern "C"
