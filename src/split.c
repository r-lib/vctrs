#include "vctrs.h"

// [[ register() ]]
SEXP vec_split(SEXP x, SEXP by) {
  if (vec_size(x) != vec_size(by)) {
    Rf_errorcall(R_NilValue, "`x` and `by` must have the same size.");
  }

  SEXP out = PROTECT(vec_group_loc(by));

  SEXP indices = VECTOR_ELT(out, 1);

  SEXP val = vec_chop(x, indices);
  SET_VECTOR_ELT(out, 1, val);

  SEXP names = PROTECT(Rf_getAttrib(out, R_NamesSymbol));
  SET_STRING_ELT(names, 1, strings_val);
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(2);
  return out;
}

