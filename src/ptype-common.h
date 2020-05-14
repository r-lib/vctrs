#ifndef VCTRS_PTYPE_COMMON_H
#define VCTRS_PTYPE_COMMON_H

#include "ptype2.h"


SEXP vec_ptype_common_params(SEXP dots,
                             SEXP ptype,
                             enum df_fallback df_fallback,
                             enum s3_fallback s3_fallback);

SEXP vec_ptype_common_opts(SEXP dots,
                           SEXP ptype,
                           const struct fallback_opts* opts);


#endif
