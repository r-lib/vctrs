#include "vctrs.h"

bool is_data_frame(SEXP x) {
  return Rf_inherits(x, "data.frame");
}

bool is_record(SEXP x) {
  return Rf_inherits(x, "vctrs_rcrd");
}

bool is_scalar(SEXP x) {
  return Rf_inherits(x, "vctrs_sclr");
}
