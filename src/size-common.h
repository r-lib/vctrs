#ifndef VCTRS_SIZE_COMMON_H
#define VCTRS_SIZE_COMMON_H

#include "vctrs-core.h"

r_ssize vec_size_common(
  r_obj* xs,
  r_ssize absent,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

r_obj* vec_recycle_common(
  r_obj* xs,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
);

#endif
