#include "vctrs.h"
#include "utils.h"

// Defined below
enum vctrs_s3_class_type s3_class_type(SEXP x);
static enum vctrs_s3_class_type s3_class_type_impl(SEXP cls);
static const char* s3_class_type_as_str(enum vctrs_s3_class_type type);

// [[ register() ]]
SEXP vctrs_s3_class_type(SEXP x) {
  return Rf_mkString(s3_class_type_as_str(s3_class_type(x)));
}

// [[ include("vctrs.h") ]]
enum vctrs_s3_class_type s3_class_type(SEXP x) {
  SEXP cls = PROTECT(Rf_getAttrib(x, R_ClassSymbol));

  if (cls == R_NilValue) {
    Rf_errorcall(R_NilValue, "Internal error: Non-S3 type detected in `s3_class_type()`");
  }

  UNPROTECT(1);
  return s3_class_type_impl(cls);
}

// Currently hard codes a few known S3 types that we have internal support for
static enum vctrs_s3_class_type s3_class_type_impl(SEXP cls) {
  R_len_t size = Rf_length(cls);
  const SEXP* p_cls = STRING_PTR(cls);

  switch(size) {
  case 1: {
    if (p_cls[0] == strings_factor) {
      return vctrs_s3_class_bare_factor;
    }
    break;
  }
  case 2: {
    if (p_cls[0] == strings_ordered &&
        p_cls[1] == strings_factor) {
      return vctrs_s3_class_bare_ordered;
    }
    break;
  }
  }

  return vctrs_s3_class_unknown;
}

static const char* s3_class_type_as_str(enum vctrs_s3_class_type type) {
  switch (type) {
  case vctrs_s3_class_bare_factor: return "bare_factor";
  case vctrs_s3_class_bare_ordered: return "bare_ordered";
  case vctrs_s3_class_unknown: return "unknown";
  }
  never_reached("s3_class_type_as_str");
}
