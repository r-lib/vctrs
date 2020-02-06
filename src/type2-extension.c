#include "vctrs.h"
#include "utils.h"

static SEXP fct_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg);
static SEXP ord_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg);

// [[ include("vctrs.h") ]]
SEXP vec_ptype2_extension(SEXP x, SEXP y,
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


static SEXP levels_union(SEXP x, SEXP y);

static SEXP fct_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP x_levels = Rf_getAttrib(x, R_LevelsSymbol);
  SEXP y_levels = Rf_getAttrib(y, R_LevelsSymbol);

  if (TYPEOF(x_levels) != STRSXP) {
    stop_corrupt_factor_levels(x, x_arg);
  }

  if (TYPEOF(y_levels) != STRSXP) {
    stop_corrupt_factor_levels(y, y_arg);
  }

  // Quick early exit for identical levels pointing to the same SEXP
  if (x_levels == y_levels) {
    return new_empty_factor(x_levels);
  }

  SEXP levels = PROTECT(levels_union(x_levels, y_levels));

  SEXP out = new_empty_factor(levels);

  UNPROTECT(1);
  return out;
}

static SEXP ord_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP x_levels = Rf_getAttrib(x, R_LevelsSymbol);
  SEXP y_levels = Rf_getAttrib(y, R_LevelsSymbol);

  if (TYPEOF(x_levels) != STRSXP) {
    stop_corrupt_ordered_levels(x, x_arg);
  }

  if (TYPEOF(y_levels) != STRSXP) {
    stop_corrupt_ordered_levels(y, y_arg);
  }

  // Quick early exit for identical levels pointing to the same SEXP
  if (x_levels == y_levels) {
    return new_empty_ordered(x_levels);
  }

  SEXP levels = PROTECT(levels_union(x_levels, y_levels));

  SEXP out = new_empty_ordered(levels);

  UNPROTECT(1);
  return out;
}

static SEXP levels_union(SEXP x, SEXP y) {
  SEXP args = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(args, 0, x);
  SET_VECTOR_ELT(args, 1, y);

  // Combine with known ptype
  // No name repair because this is just combining factor levels
  SEXP xy = PROTECT(vec_c(
    args,
    vctrs_shared_empty_chr,
    R_NilValue,
    name_repair_none
  ));

  SEXP out = vec_unique(xy);

  UNPROTECT(2);
  return out;
}
