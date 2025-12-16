#ifndef VCTRS_PTYPE2_H
#define VCTRS_PTYPE2_H

#include "vctrs-core.h"

// Sync with R constants in ptype2.R
enum s3_fallback {
  S3_FALLBACK_false = 0,
  S3_FALLBACK_true = 1
};

r_obj* vec_ptype2(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback,
  int* left
);

bool vec_is_coercible(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback
);

enum s3_fallback s3_fallback_from_opts(r_obj* opts);

r_obj* vec_ptype2_from_unspecified(
  r_obj* x,
  struct vctrs_arg* p_x_arg,
  enum vctrs_type x_type,
  struct r_lazy call,
  enum s3_fallback s3_fallback
);

#endif
