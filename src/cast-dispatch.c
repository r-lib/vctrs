#include "vctrs.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP vec_cast_dispatch(SEXP x,
                       SEXP to,
                       enum vctrs_type x_type,
                       enum vctrs_type to_type,
                       bool* lossy,
                       struct vctrs_arg* x_arg,
                       struct vctrs_arg* to_arg) {
  int dir = 0;
  enum vctrs_type2_s3 type2_s3 = vec_typeof2_s3_impl(x, to, x_type, to_type, &dir);

  switch (type2_s3) {

  case vctrs_type2_s3_character_bare_factor:
    if (dir == 0) {
      return chr_as_factor(x, to, lossy, to_arg);
    } else {
      return fct_as_character(x, x_arg);
    }

  case vctrs_type2_s3_character_bare_ordered:
    if (dir == 0) {
      return chr_as_ordered(x, to, lossy, to_arg);
    } else {
      return ord_as_character(x, x_arg);
    }

  case vctrs_type2_s3_bare_factor_bare_factor:
    return fct_as_factor(x, to, lossy, x_arg, to_arg);

  case vctrs_type2_s3_bare_ordered_bare_ordered:
    return ord_as_ordered(x, to, lossy, x_arg, to_arg);

  case vctrs_type2_s3_dataframe_bare_tibble:
    if (dir == 0) {
      return tib_cast(x, to, x_arg, to_arg);
    } else {
      return df_cast(x, to, x_arg, to_arg);
    }

  default:
    return R_NilValue;
  }
}
