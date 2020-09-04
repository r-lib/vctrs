#ifndef VCTRS_COMPARATOR_H
#define VCTRS_COMPARATOR_H

#include "vctrs.h"

struct comparator {
  int (*equal)(const void* x, R_len_t i, const void* y, R_len_t j);
  int (*equal_missing)(const void* x, R_len_t i);
  SEXP self;
};

struct comparator_vec {
  SEXP vec;
  const void* vec_p;
  SEXP self;
};

#define PROTECT_COMPARATOR(p_comparator, p_n) do { \
  PROTECT((p_comparator)->self);                   \
  *(p_n) += 1;                                     \
} while(0)

#define PROTECT_COMPARATOR_VEC(p_comparator_vec, p_n) do { \
  PROTECT((p_comparator_vec)->vec);                        \
  PROTECT((p_comparator_vec)->self);                       \
  *(p_n) += 2;                                             \
} while(0)

struct comparator* new_comparator(SEXP x);
struct comparator_vec* new_comparator_vec(SEXP x);

#endif
