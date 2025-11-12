#ifndef VCTRS_UNSPECIFIED_H
#define VCTRS_UNSPECIFIED_H

#include "vctrs-core.h"

SEXP vec_unspecified(R_len_t n);
bool vec_is_unspecified(SEXP x);

enum ptype_finalise {
  PTYPE_FINALISE_false,
  PTYPE_FINALISE_true
};

#define PTYPE_FINALISE_DEFAULT PTYPE_FINALISE_true

static inline
bool should_finalise(enum ptype_finalise finalise) {
  return finalise == PTYPE_FINALISE_true;
}

r_obj* vec_ptype_finalise(r_obj* x);

#endif
