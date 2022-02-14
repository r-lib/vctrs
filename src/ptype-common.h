#ifndef VCTRS_PTYPE_COMMON_H
#define VCTRS_PTYPE_COMMON_H

#include "vctrs-core.h"
#include "ptype2.h"
#include "utils.h"

static inline
bool vec_is_common_class_fallback(r_obj* ptype) {
  return r_inherits(ptype, c_strs_vctrs_common_class_fallback);
}

r_obj* vec_ptype_common_params(r_obj* dots,
                               r_obj* ptype,
                               enum df_fallback df_fallback,
                               enum s3_fallback s3_fallback);

r_obj* vec_ptype_common_opts(r_obj* dots,
                             r_obj* ptype,
                             const struct fallback_opts* opts);

#endif
