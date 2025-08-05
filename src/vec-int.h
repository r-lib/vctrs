#ifndef VCTRS_VEC_INT_H
#define VCTRS_VEC_INT_H

#include <rlang.h>

static inline
bool r_int_all(r_obj* x, int value) {
  const r_ssize size = r_length(x);
  const int* v_x = r_int_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i] != value) {
      return false;
    }
  }

  return true;
}

static inline
r_ssize r_int_count_complete(r_obj* x) {
  const int* v_x = r_int_cbegin(x);
  const r_ssize size = r_length(x);

  r_ssize out = 0;

  for (r_ssize i = 0; i < size; ++i) {
    out += (v_x[i] != r_globals.na_int);
  }

  return out;
}

static inline
r_obj* r_int_locate_complete(r_obj* x) {
  const int* v_x = r_int_cbegin(x);
  const r_ssize x_size = r_length(x);

  const r_ssize out_size = r_int_count_complete(x);
  r_obj* out = KEEP(r_alloc_integer(out_size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0, j = 0; i < x_size && j < out_size; ++i) {
    v_out[j] = i + 1;
    j += (v_x[i] != r_globals.na_int);
  }

  FREE(1);
  return out;
}

#endif
