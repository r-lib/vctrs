#include "vctrs.h"

bool is_data_frame(SEXP x) {
  return Rf_inherits(x, "data.frame");
}

bool is_record(SEXP x) {
  return Rf_inherits(x, "vctrs_rcrd") || Rf_inherits(x, "POSIXlt");
}

bool is_scalar(SEXP x) {
  return Rf_inherits(x, "vctrs_sclr");
}

enum vctrs_type vec_typeof(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP: return vctrs_type_logical;
  case INTSXP: return vctrs_type_integer;
  case REALSXP: return vctrs_type_double;
  case CPLXSXP: return vctrs_type_double;
  case STRSXP: return vctrs_type_character;
  case RAWSXP: return vctrs_type_raw;
  case VECSXP:
    if (is_data_frame(x)) {
      return vctrs_type_dataframe;
    } else {
      return vctrs_type_list;
    }
  default:
    Rf_error("Unsupported type", Rf_type2char(TYPEOF(x)));
  }
}
