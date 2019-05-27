#include "vctrs.h"
#include "utils.h"


static SEXP as_df_row(SEXP x, bool quiet) {
  if (x == R_NilValue) {
    return x;
  }
  if (vec_is_unspecified(x) && r_names(x) == R_NilValue) {
    return x;
  }
  if (is_data_frame(x)) {
    return x;
  }

  if (vec_dim(x) != 1) {
    return r_as_data_frame(x);
  }

  x = PROTECT(r_as_list(x));

  SEXP nms = PROTECT(vec_unique_names(x, quiet));
  Rf_setAttrib(x, R_NamesSymbol, nms);

  x = new_data_frame(x, 1);

  UNPROTECT(2);
  return x;
}

// [[ register() ]]
SEXP vctrs_as_df_row(SEXP x, SEXP quiet) {
  return as_df_row(x, LOGICAL(quiet)[0]);
}
