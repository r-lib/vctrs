#ifndef VCTRS_PTYPE_COMMON_H
#define VCTRS_PTYPE_COMMON_H

#include "vctrs-core.h"
#include "ptype2.h"
#include "utils.h"

static inline
bool vec_is_common_class_fallback(SEXP ptype) {
  return Rf_inherits(ptype, c_strs_vctrs_common_class_fallback);
}

SEXP vec_ptype_common_params(SEXP dots,
                             SEXP ptype,
                             enum df_fallback df_fallback,
                             enum s3_fallback s3_fallback);

SEXP vec_ptype_common_opts(SEXP dots,
                           SEXP ptype,
                           const struct fallback_opts* opts);

#endif
