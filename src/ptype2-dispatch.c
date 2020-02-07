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

// Initialised at load time
static SEXP fns_vec_type2_dispatch = NULL;
static SEXP syms_vec_type2_dispatch = NULL;

SEXP vctrs_type2_dispatch(SEXP x,
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

void vctrs_init_type2(SEXP ns) {
  syms_vec_type2_dispatch = Rf_install("vec_type2_dispatch");
  fns_vec_type2_dispatch = Rf_findVar(syms_vec_type2_dispatch, ns);
}
