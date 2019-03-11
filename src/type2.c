#include "vctrs.h"


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
  dispatch: {
    SEXP dispatch_call = PROTECT(Rf_lang3(vec_type2_dispatch_fn, x, y));
    SEXP out = Rf_eval(dispatch_call, R_GlobalEnv);

    UNPROTECT(1);
    return out;
  }}
}


void vctrs_init_type2(SEXP ns) {
  vec_type2_dispatch_fn = Rf_findVar(Rf_install("vec_type2_dispatch"), ns);
}
