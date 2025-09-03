#ifndef VCTRS_C_H
#define VCTRS_C_H

#include "vctrs-core.h"
#include "names.h"

r_obj* vec_c(
  r_obj* xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* name_repair,
  struct vctrs_arg* p_error_arg,
  struct r_lazy error_call
);

#endif
