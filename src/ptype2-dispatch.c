#include "vctrs.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_dispatch(SEXP x, SEXP y,
                         enum vctrs_type x_type,
                         enum vctrs_type y_type,
                         struct vctrs_arg* x_arg,
                         struct vctrs_arg* y_arg,
                         int* left) {
  enum vctrs_type2_s3 type2_s3 = vec_typeof2_s3_impl(x, y, x_type, y_type, left);

  switch (type2_s3) {
  case vctrs_type2_s3_character_bare_factor:
  case vctrs_type2_s3_character_bare_ordered:
    return vctrs_shared_empty_chr;

  case vctrs_type2_s3_bare_factor_bare_factor:
    return fct_ptype2(x, y, x_arg, y_arg);

  case vctrs_type2_s3_bare_ordered_bare_ordered:
    return ord_ptype2(x, y, x_arg, y_arg);

  case vctrs_type2_s3_bare_date_bare_date:
    return vctrs_shared_empty_date;

  case vctrs_type2_s3_bare_date_bare_posixct:
  case vctrs_type2_s3_bare_date_bare_posixlt:
    return date_datetime_ptype2(x, y);

  case vctrs_type2_s3_bare_posixct_bare_posixct:
  case vctrs_type2_s3_bare_posixct_bare_posixlt:
  case vctrs_type2_s3_bare_posixlt_bare_posixlt:
    return datetime_datetime_ptype2(x, y);

  case vctrs_type2_s3_dataframe_bare_tibble:
  case vctrs_type2_s3_bare_tibble_bare_tibble:
    return tibble_ptype2(x, y, x_arg, y_arg);

  default:
    return vec_ptype2_dispatch_s3(x, y, x_arg, y_arg);
  }
}

// Initialised at load time
static SEXP fns_vec_ptype2_dispatch_s3 = NULL;
static SEXP syms_vec_ptype2_dispatch_s3 = NULL;

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_dispatch_s3(SEXP x,
                            SEXP y,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg) {
                            SEXP x_arg_chr = PROTECT(vctrs_arg(x_arg));
  SEXP y_arg_chr = PROTECT(vctrs_arg(y_arg));

  SEXP syms[5] = { syms_x, syms_y, syms_x_arg, syms_y_arg, NULL };
  SEXP args[5] = {      x,      y,  x_arg_chr,  y_arg_chr, NULL };

  SEXP out = vctrs_dispatch_n(syms_vec_ptype2_dispatch_s3, fns_vec_ptype2_dispatch_s3,
                              syms, args);

  UNPROTECT(2);
  return out;
}

void vctrs_init_ptype2_dispatch(SEXP ns) {
  syms_vec_ptype2_dispatch_s3 = Rf_install("vec_ptype2_dispatch_s3");
  fns_vec_ptype2_dispatch_s3 = Rf_findVar(syms_vec_ptype2_dispatch_s3, ns);
}
