#include "vctrs.h"

// [[ register() ]]
SEXP vctrs_dim(SEXP x) {
  return vec_dim(x);
}

// [[ register() ]]
SEXP vctrs_dim_n(SEXP x) {
  return r_int(vec_dim_n(x));
}

// [[ register() ]]
SEXP vctrs_has_dim(SEXP x) {
  return r_lgl(has_dim(x));
}
