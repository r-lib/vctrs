#ifndef VCTRS_PTYPE_COMMON_H
#define VCTRS_PTYPE_COMMON_H

#include "vctrs-core.h"
#include "ptype2.h"
#include "utils.h"

struct ptype_common_opts {
  struct r_lazy call;
  struct vctrs_arg* p_arg;
  enum s3_fallback s3_fallback;
};

static inline
bool vec_is_common_class_fallback(r_obj* ptype) {
  return r_inherits(ptype, c_strs_vctrs_common_class_fallback);
}

r_obj* vec_ptype_common(
  r_obj* dots,
  r_obj* ptype,
  enum s3_fallback s3_fallback,
  struct vctrs_arg* p_arg,
  struct r_lazy call
);

r_obj* vec_ptype_common_opts(r_obj* dots,
                             r_obj* ptype,
                             const struct ptype_common_opts* opts);

#endif
