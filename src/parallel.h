#ifndef VCTRS_PARALLEL_H
#define VCTRS_PARALLEL_H

#include "vctrs-core.h"

enum list_parallel_missing {
  LIST_PARALLEL_MISSING_na,
  LIST_PARALLEL_MISSING_false,
  LIST_PARALLEL_MISSING_true
};

r_obj* list_pany(
  r_obj* xs,
  enum list_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

r_obj* list_pall(
  r_obj* xs,
  enum list_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

#endif
