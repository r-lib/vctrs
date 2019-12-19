#include "vctrs.h"
#include "slice.h"
#include "utils.h"

// Initialised at load time
static SEXP syms_tbl_cast_dispatch = NULL;
static SEXP fns_tbl_cast_dispatch = NULL;


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

  SEXP sliced_proxy = PROTECT(list_slice(proxy, index));
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


// [[ include("vctrs.h") ]]
SEXP tbl_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  if (x == R_NilValue || to == R_NilValue) {
    return x;
  }

  SEXP out = vctrs_dispatch4(syms_tbl_cast_dispatch, fns_tbl_cast_dispatch,
                             syms_x, x,
                             syms_to, to,
                             syms_x_arg, PROTECT(vctrs_arg(x_arg)),
                             syms_to_arg, PROTECT(vctrs_arg(to_arg)));
  UNPROTECT(2);
  return out;
}
// [[ register() ]]
SEXP vctrs_tbl_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  if (!r_is_string(x_arg_)) {
    Rf_errorcall(R_NilValue, "`x_arg` must be a string");
  }
  if (!r_is_string(to_arg_)) {
    Rf_errorcall(R_NilValue, "`to_arg` must be a string");
  }

  struct vctrs_arg x_arg = new_wrapper_arg(NULL, r_chr_get_c_string(x_arg_, 0));
  struct vctrs_arg to_arg = new_wrapper_arg(NULL, r_chr_get_c_string(to_arg_, 0));

  return tbl_cast(x, to, &x_arg, &to_arg);
}


void vctrs_init_table(SEXP ns) {
  syms_tbl_cast_dispatch = Rf_install("tbl_cast_dispatch");
  fns_tbl_cast_dispatch = Rf_findVar(syms_tbl_cast_dispatch, ns);
}
