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

const char* vec_type_as_str(enum vctrs_type type) {
  switch (type) {
  case vctrs_type_null:      return "null";
  case vctrs_type_logical:   return "logical";
  case vctrs_type_integer:   return "integer";
  case vctrs_type_double:    return "double";
  case vctrs_type_complex:   return "complex";
  case vctrs_type_character: return "character";
  case vctrs_type_raw:       return "raw";
  case vctrs_type_list:      return "list";
  case vctrs_type_dataframe: return "dataframe";
  }
}

void vctrs_stop_unsupported_type(enum vctrs_type type, const char* fn) {
  Rf_errorcall(R_NilValue,
               "Unsupported vctrs type `%s` in `%s`",
               vec_type_as_str(type),
               fn);
}
