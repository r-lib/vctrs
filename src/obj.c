#define R_NO_REMAP
#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>

SEXP obj_set_(SEXP x, SEXP val_) {
  Rboolean val = Rf_asLogical(val_);

  Rboolean old = OBJECT(x);
  SET_OBJECT(x, val);

  return Rf_ScalarLogical(old);
}

SEXP obj_get_(SEXP x) {
  return Rf_ScalarLogical(OBJECT(x));
}
