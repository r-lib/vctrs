#ifndef VCTRS_UNSPECIFIED_H
#define VCTRS_UNSPECIFIED_H

#include "vctrs-core.h"

SEXP vec_unspecified(R_len_t n);
bool vec_is_unspecified(SEXP x);

r_obj* vec_ptype_finalise(r_obj* x);

#endif
