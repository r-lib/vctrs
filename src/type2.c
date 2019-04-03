#include "vctrs.h"
#include "utils.h"


// Initialised at load time
static SEXP fns_vec_type2_dispatch = NULL;
static SEXP syms_vec_type2_dispatch = NULL;

static SEXP vctrs_type2_dispatch(SEXP x, SEXP y) {
  return vctrs_dispatch2(syms_vec_type2_dispatch, fns_vec_type2_dispatch,
                         syms_x, x,
                         syms_y, y);
}

SEXP vctrs_type2(SEXP x, SEXP y) {
  if (has_dim(x) || has_dim(y)) {
    return vctrs_type2_dispatch(x, y);
  }

  enum vctrs_type type_x = vec_typeof(x);
  enum vctrs_type type_y = vec_typeof(y);

  if (type_x == vctrs_type_scalar) {
    stop_scalar_type(x, "x");
  }
  if (type_y == vctrs_type_scalar) {
    stop_scalar_type(y, "y");
  }

  switch (vec_typeof2_impl(type_x, type_y)) {
  case vctrs_type2_null_null:
    return R_NilValue;

  case vctrs_type2_logical_logical:
    return vctrs_shared_empty_lgl;

  case vctrs_type2_logical_integer:
  case vctrs_type2_integer_integer:
    return vctrs_shared_empty_int;

  case vctrs_type2_logical_double:
  case vctrs_type2_integer_double:
  case vctrs_type2_double_double:
    return vctrs_shared_empty_dbl;

  case vctrs_type2_character_character:
    return vctrs_shared_empty_chr;

  case vctrs_type2_raw_raw:
    return vctrs_shared_empty_raw;

  case vctrs_type2_list_list:
    return vctrs_shared_empty_list;

  default:
    return vctrs_type2_dispatch(x, y);
  }
}


void vctrs_init_type2(SEXP ns) {
  syms_vec_type2_dispatch = Rf_install("vec_type2_dispatch");
  fns_vec_type2_dispatch = Rf_findVar(syms_vec_type2_dispatch, ns);
}
