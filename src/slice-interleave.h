#ifndef VCTRS_SLICE_INTERLEAVE_H
#define VCTRS_SLICE_INTERLEAVE_H

#include "vctrs-core.h"
#include "names.h"

r_obj* list_interleave(
  r_obj* x,
  r_ssize size,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_x_arg,
  struct r_lazy error_call
);

#endif
