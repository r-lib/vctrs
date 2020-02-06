#include "vctrs.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_dispatch(SEXP x, SEXP y,
                         enum vctrs_type x_type,
                         enum vctrs_type y_type,
                         struct vctrs_arg* x_arg,
                         struct vctrs_arg* y_arg,
                         int* left) {
  enum vctrs_s3_type2 s3_type2 = vec_s3_typeof2_impl(x, y, x_type, y_type, left);

  switch (s3_type2) {
  case vctrs_s3_type2_character_bare_factor:
  case vctrs_s3_type2_character_bare_ordered:
    return vctrs_shared_empty_chr;

  case vctrs_s3_type2_bare_factor_bare_factor:
    return fct_ptype2(x, y, x_arg, y_arg);

  case vctrs_s3_type2_bare_ordered_bare_ordered:
    return ord_ptype2(x, y, x_arg, y_arg);

  default:
    return vctrs_type2_dispatch(x, y, x_arg, y_arg);
  }
}
