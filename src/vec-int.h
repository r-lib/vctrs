#ifndef VCTRS_VEC_INT_H
#define VCTRS_VEC_INT_H

#include <rlang.h>

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

#endif
