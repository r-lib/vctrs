#ifndef VCTRS_ALTREP_REP_H
#define VCTRS_ALTREP_REP_H

#include "altrep.h"

bool vec_is_altrep_vctrs_compact_rep(SEXP x);

SEXP new_altrep_vctrs_compact_intrep(int value, R_xlen_t size);

#if (R_VERSION >= R_Version(3, 5, 0))

R_altrep_class_t altrep_vctrs_compact_intrep_class;

#endif

#endif
