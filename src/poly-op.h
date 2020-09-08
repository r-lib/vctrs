#ifndef VCTRS_POLY_OP
#define VCTRS_POLY_OP

#include "vctrs.h"

typedef int (*poly_binary_op_fn_t)(const void* x, R_len_t i, const void* y, R_len_t j);
poly_binary_op_fn_t new_poly_op_equal_scalar_na_equal_p(SEXP proxy);

typedef int (*poly_unary_op_fn_t)(const void* x, R_len_t i);
poly_unary_op_fn_t new_poly_op_equal_missing_p(SEXP proxy);


struct poly_vec {
  SEXP vec;
  const void* p_vec;
  SEXP self;
};

struct poly_vec* new_poly_vec(SEXP proxy);

#define PROTECT_POLY_VEC(p_poly_vec, p_n) do { \
  PROTECT((p_poly_vec)->vec);                  \
  PROTECT((p_poly_vec)->self);                 \
  *(p_n) += 2;                                 \
} while(0)

#endif
