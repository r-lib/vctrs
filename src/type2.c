#include "vctrs.h"
#include "utils.h"


// Initialised at load time
static SEXP fns_vec_type2_dispatch = NULL;
static SEXP syms_vec_type2_dispatch = NULL;

static SEXP vctrs_type2_dispatch(SEXP x,
                                 SEXP y,
                                 struct vctrs_arg* x_arg,
                                 struct vctrs_arg* y_arg) {
  SEXP x_arg_chr = PROTECT(vctrs_arg(x_arg));
  SEXP y_arg_chr = PROTECT(vctrs_arg(y_arg));

  SEXP syms[5] = { syms_x, syms_y, syms_x_arg, syms_y_arg, NULL };
  SEXP args[5] = {      x,      y,  x_arg_chr,  y_arg_chr, NULL };

  SEXP out = vctrs_dispatch_n(syms_vec_type2_dispatch, fns_vec_type2_dispatch,
                              syms, args);

  UNPROTECT(2);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_type2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  if (x == R_NilValue) {
    if (!vec_is_partial(y)) {
      vec_assert(y, y_arg);
    }
    return y;
  }
  if (y == R_NilValue) {
    if (!vec_is_partial(x)) {
      vec_assert(x, x_arg);
    }
    return x;
  }

  if (has_dim(x) || has_dim(y)) {
    return vctrs_type2_dispatch(x, y, x_arg, y_arg);
  }

  enum vctrs_type type_x = vec_typeof(x);
  enum vctrs_type type_y = vec_typeof(y);

  if (type_x == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg);
  }
  if (type_y == vctrs_type_scalar) {
    stop_scalar_type(y, y_arg);
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
    return vctrs_type2_dispatch(x, y, x_arg, y_arg);
  }
}

// [[ register() ]]
SEXP vctrs_type2(SEXP x, SEXP y, SEXP x_arg, SEXP y_arg) {
  if (x_arg == R_NilValue) {
    x_arg = vctrs_shared_empty_str;
  } else if (!r_is_string(x_arg)) {
    Rf_errorcall(R_NilValue, "`x_arg` must be a string");
  }

  if (y_arg == R_NilValue) {
    y_arg = vctrs_shared_empty_str;
  } else if (!r_is_string(y_arg)) {
    Rf_errorcall(R_NilValue, "`y_arg` must be a string");
  }

  struct vctrs_arg_wrapper x_arg_ = new_vctrs_arg(NULL, r_chr_get_c_string(x_arg, 0));
  struct vctrs_arg_wrapper y_arg_ = new_vctrs_arg(NULL, r_chr_get_c_string(y_arg, 0));

  return vec_type2(x, y, (struct vctrs_arg*) &x_arg_, (struct vctrs_arg*) &y_arg_);
}

void vctrs_init_type2(SEXP ns) {
  syms_vec_type2_dispatch = Rf_install("vec_type2_dispatch");
  fns_vec_type2_dispatch = Rf_findVar(syms_vec_type2_dispatch, ns);
}
