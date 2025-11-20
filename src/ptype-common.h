#ifndef VCTRS_PTYPE_COMMON_H
#define VCTRS_PTYPE_COMMON_H

#include "vctrs-core.h"
#include "ptype2.h"
#include "unspecified.h"
#include "utils.h"

static inline
bool vec_is_common_class_fallback(r_obj* ptype) {
  return r_inherits(ptype, c_strs_vctrs_common_class_fallback);
}

r_obj* vec_ptype_common(
  r_obj* dots,
  r_obj* ptype,
  enum ptype_finalise finalise,
  enum s3_fallback s3_fallback,
  struct vctrs_arg* p_arg,
  struct r_lazy call
);

#endif
