#include "vctrs.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP tib_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP out = PROTECT(df_ptype2(x, y, x_arg, y_arg));

  Rf_setAttrib(out, R_ClassSymbol, classes_tibble);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_tib_ptype2(SEXP x, SEXP y, SEXP x_arg_, SEXP y_arg_) {
  struct vctrs_arg x_arg = new_wrapper_arg(NULL, r_chr_get_c_string(x_arg_, 0));
  struct vctrs_arg y_arg = new_wrapper_arg(NULL, r_chr_get_c_string(y_arg_, 0));
  return tib_ptype2(x, y, &x_arg, &y_arg);
}
