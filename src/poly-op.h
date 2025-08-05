#ifndef VCTRS_POLY_OP
#define VCTRS_POLY_OP

#include "vctrs-core.h"


struct poly_vec {
  r_obj* shelter;
  r_obj* vec;
  const void* p_vec;
  enum vctrs_type type;
};

struct poly_vec* new_poly_vec(r_obj* proxy, enum vctrs_type type);


struct poly_df_data {
  enum vctrs_type* v_col_type;
  const void** v_col_ptr;
  r_ssize n_col;
};


typedef int (poly_binary_int_fn)(const void* x, r_ssize i, const void* y, r_ssize j);
poly_binary_int_fn* poly_p_compare_na_equal(enum vctrs_type type);

typedef bool (poly_unary_bool_fn)(const void* x, r_ssize i);
poly_unary_bool_fn* poly_p_is_missing(enum vctrs_type type);


#endif
