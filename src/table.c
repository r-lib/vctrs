#include "vctrs.h"
#include "slice.h"
#include "utils.h"


// Currently only data frames are considered tabular. Eventually
// we should also allow arrays of dimensionality >= 2.

// [[ include("vctrs.h") ]]
bool vec_is_tabular(SEXP x) {
  return is_data_frame(x);
}

// [[ include("vctrs.h") ]]
void tbl_assert(SEXP x) {
  if (!vec_is_tabular(x)) {
    Rf_error("Input must be a data frame.");
  }
}
// [[ register() ]]
SEXP vctrs_tbl_assert(SEXP x) {
  tbl_assert(x);
  return R_NilValue;
}
// [[ register() ]]
SEXP vctrs_tbl_is(SEXP x) {
  return Rf_ScalarLogical(vec_is_tabular(x));
}


// [[ include("vctrs.h"); register() ]]
SEXP tbl_slice(SEXP x, SEXP index) {
  tbl_assert(x);
  SEXP proxy = PROTECT(vec_proxy(x));

  if (TYPEOF(proxy) != VECSXP) {
    Rf_error("Internal error: Expected list in tabular proxy.");
  }

  SEXP names = PROTECT(r_names(x));
  index = PROTECT(vec_as_index(index, vec_size(x), names));

  SEXP sliced_proxy = PROTECT(list_slice(x, index));
  Rf_copyMostAttrib(sliced_proxy, proxy);

  names = PROTECT(slice_names(names, index));
  Rf_setAttrib(sliced_proxy, R_NamesSymbol, names);

  SEXP row_names = PROTECT(get_rownames(x));
  Rf_setAttrib(sliced_proxy, R_RowNamesSymbol, row_names);

  SEXP out = vec_restore(sliced_proxy, x, R_NilValue);

  UNPROTECT(6);
  return out;
}

// [[ include("vctrs.h"); register() ]]
SEXP tbl_ptype(SEXP x) {
  SEXP out = PROTECT(tbl_slice(x, vctrs_shared_empty_int));
  out = vec_slice(out, vctrs_shared_empty_int);
  UNPROTECT(1);
  return out;
}
