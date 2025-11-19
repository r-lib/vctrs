#ifndef VCTRS_PARALLEL_H
#define VCTRS_PARALLEL_H

#include "vctrs-core.h"

enum vec_parallel_missing {
  VEC_PARALLEL_MISSING_na,
  VEC_PARALLEL_MISSING_false,
  VEC_PARALLEL_MISSING_true
};

r_obj* vec_pany(
  r_obj* xs,
  enum vec_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

r_obj* vec_pall(
  r_obj* xs,
  enum vec_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

#endif
