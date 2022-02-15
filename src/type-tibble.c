#include "vctrs.h"
#include "type-data-frame.h"

// [[ include("vctrs.h") ]]
SEXP tib_ptype2(const struct ptype2_opts* opts) {
  SEXP out = PROTECT(df_ptype2(opts));

  Rf_setAttrib(out, R_ClassSymbol, classes_tibble);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_tib_ptype2(SEXP x, SEXP y, SEXP x_arg_, SEXP y_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg y_arg = vec_as_arg(y_arg_);

  const struct ptype2_opts opts = {
    .x = x,
    .y = y,
    .x_arg = &x_arg,
    .y_arg = &y_arg
  };

  return tib_ptype2(&opts);
}

// [[ include("type-tibble.h") ]]
SEXP tib_cast(const struct cast_opts* opts) {
  SEXP out = PROTECT(df_cast_opts(opts));

  Rf_setAttrib(out, R_ClassSymbol, classes_tibble);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_tib_cast(SEXP x, SEXP to, SEXP x_arg, SEXP to_arg) {
  struct vctrs_arg c_x_arg = vec_as_arg(x_arg);
  struct vctrs_arg c_to_arg = vec_as_arg(to_arg);

  const struct cast_opts opts = {
    .x = x,
    .to = to,
    .x_arg = &c_x_arg,
    .to_arg = &c_to_arg
  };

  return tib_cast(&opts);
}
