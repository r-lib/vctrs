#include "vctrs.h"
#include "utils.h"


// Initialised at load time
static SEXP vec_type2_dispatch_fn = NULL;

SEXP vctrs_type2(SEXP x, SEXP y) {
  if (has_dim(x) || has_dim(y)) {
    goto dispatch;
  }

  switch (vec_dispatch_typeof(x, y)) {
  case vctrs_dispatch_null_null:
    return R_NilValue;

  case vctrs_dispatch_logical_logical:
    return vctrs_shared_empty_lgl;

  case vctrs_dispatch_logical_integer:
  case vctrs_dispatch_integer_integer:
    return vctrs_shared_empty_int;

  case vctrs_dispatch_logical_double:
  case vctrs_dispatch_integer_double:
  case vctrs_dispatch_double_double:
    return vctrs_shared_empty_dbl;

  case vctrs_dispatch_character_character:
    return vctrs_shared_empty_chr;

  case vctrs_dispatch_raw_raw:
    return vctrs_shared_empty_raw;

  case vctrs_dispatch_list_list:
    return vctrs_shared_empty_list;

  default:
  dispatch:
    return vctrs_dispatch2(vec_type2_dispatch_fn, R_NilValue, x, R_NilValue, y, R_GlobalEnv);
  }
}


void vctrs_init_type2(SEXP ns) {
  vec_type2_dispatch_fn = Rf_findVar(Rf_install("vec_type2_dispatch"), ns);
}
