#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "arg-counter.h"

// Initialised at load time
static SEXP syms_vec_is_vector_dispatch = NULL;
static SEXP fns_vec_is_vector_dispatch = NULL;

// From proxy.c
SEXP vec_proxy_method(SEXP x);
SEXP vec_proxy_invoke(SEXP x, SEXP method);


static enum vctrs_type vec_base_typeof(SEXP x, bool proxied);

// [[ include("vctrs.h") ]]
struct vctrs_type_info vec_type_info(SEXP x) {
  struct vctrs_type_info info;

  info.type = vec_typeof(x);

  switch (info.type) {
  case vctrs_type_s3: info.proxy_method = vec_proxy_method(x); break;
  default: info.proxy_method = R_NilValue;
  }

  return info;
}

// [[ include("vctrs.h") ]]
struct vctrs_proxy_info vec_proxy_info(SEXP x) {
  struct vctrs_proxy_info info;

  info.proxy_method = OBJECT(x) ? vec_proxy_method(x) : R_NilValue;
  PROTECT(info.proxy_method);

  if (info.proxy_method == R_NilValue) {
    info.type = vec_base_typeof(x, false);
    info.proxy = x;
  } else {
    SEXP proxy = PROTECT(vec_proxy_invoke(x, info.proxy_method));
    info.type = vec_base_typeof(proxy, true);
    info.proxy = proxy;
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return info;
}

// [[ register() ]]
SEXP vctrs_type_info(SEXP x) {
  struct vctrs_type_info info = vec_type_info(x);

  SEXP out = PROTECT(Rf_mkNamed(VECSXP, (const char*[]) { "type", "proxy_method", "" }));
  SET_VECTOR_ELT(out, 0, Rf_mkString(vec_type_as_str(info.type)));
  SET_VECTOR_ELT(out, 1, info.proxy_method);

  UNPROTECT(1);
  return out;
}
// [[ register() ]]
SEXP vctrs_proxy_info(SEXP x) {
  struct vctrs_proxy_info info = vec_proxy_info(x);

  SEXP out = PROTECT(Rf_mkNamed(VECSXP, (const char*[]) { "type", "proxy_method", "proxy", "" }));
  SET_VECTOR_ELT(out, 0, Rf_mkString(vec_type_as_str(info.type)));
  SET_VECTOR_ELT(out, 1, info.proxy_method);
  SET_VECTOR_ELT(out, 2, info.proxy);

  UNPROTECT(1);
  return out;
}

static enum vctrs_type vec_base_typeof(SEXP x, bool proxied) {
  switch (TYPEOF(x)) {
  // Atomic types are always vectors
  case NILSXP: return vctrs_type_null;
  case LGLSXP: return vctrs_type_logical;
  case INTSXP: return vctrs_type_integer;
  case REALSXP: return vctrs_type_double;
  case CPLXSXP: return vctrs_type_complex;
  case STRSXP: return vctrs_type_character;
  case RAWSXP: return vctrs_type_raw;
  case VECSXP:
    // Bare lists and data frames are vectors
    if (!OBJECT(x)) return vctrs_type_list;
    if (is_data_frame(x)) return vctrs_type_dataframe;
    // S3 lists are only vectors if they are proxied
    if (proxied || Rf_inherits(x, "list")) return vctrs_type_list;
    // fallthrough
  default: return vctrs_type_scalar;
  }
}

// [[ include("vctrs.h") ]]
enum vctrs_type vec_proxy_typeof(SEXP x) {
  return vec_base_typeof(x, true);
}



// [[ register() ]]
SEXP vctrs_is_list(SEXP x) {
  return Rf_ScalarLogical(vec_is_list(x));
}

// [[ include("vctrs.h") ]]
bool vec_is_list(SEXP x) {
  // Require `x` to be a list internally
  if (TYPEOF(x) != VECSXP) {
    return false;
  }

  // Unclassed VECSXP are lists
  if (!OBJECT(x)) {
    return true;
  }

  // Classed VECSXP are only lists if the last class is explicitly `"list"`
  return class_type(x) == vctrs_class_list;
}


// [[ include("vctrs.h") ]]
bool vec_is_vector(SEXP x) {
  if (x == R_NilValue) {
    return false;
  }

  struct vctrs_proxy_info info = vec_proxy_info(x);
  return info.type != vctrs_type_scalar;
}
// [[ register() ]]
SEXP vctrs_is_vector(SEXP x) {
  return Rf_ScalarLogical(vec_is_vector(x));
}

static bool class_is_null(SEXP x) {
  return Rf_getAttrib(x, R_ClassSymbol) == R_NilValue;
}

// [[ include("vctrs.h") ]]
enum vctrs_type vec_typeof(SEXP x) {
  // Check for unspecified vectors before `vec_base_typeof()` which
  // allows vectors of `NA` to pass through as `vctrs_type_logical`
  if (vec_is_unspecified(x)) {
    return vctrs_type_unspecified;
  }

  if (!OBJECT(x) || class_is_null(x)) {
    return vec_base_typeof(x, false);
  }

  // Bare data frames are treated as a base atomic type. Subclasses of
  // data frames are treated as S3 to give them a chance to be proxied
  // or implement their own methods for cast, type2, etc.
  if (is_bare_data_frame(x)) {
    return vctrs_type_dataframe;
  }

  return vctrs_type_s3;
}

// [[ register() ]]
SEXP vctrs_typeof(SEXP x, SEXP dispatch) {
  enum vctrs_type type;
  if (LOGICAL(dispatch)[0]) {
    type = vec_proxy_info(x).type;
  } else {
    type = vec_typeof(x);
  }
  return Rf_mkString(vec_type_as_str(type));
}

__attribute__((noreturn))
void stop_unimplemented_vctrs_type(const char* fn, enum vctrs_type type) {
  r_stop_internal("Unsupported vctrs type `%s`.", vec_type_as_str(type));
}

const char* vec_type_as_str(enum vctrs_type type) {
  switch (type) {
  case vctrs_type_null:         return "null";
  case vctrs_type_unspecified:  return "unspecified";
  case vctrs_type_logical:      return "logical";
  case vctrs_type_integer:      return "integer";
  case vctrs_type_double:       return "double";
  case vctrs_type_complex:      return "complex";
  case vctrs_type_character:    return "character";
  case vctrs_type_raw:          return "raw";
  case vctrs_type_list:         return "list";
  case vctrs_type_dataframe:    return "dataframe";
  case vctrs_type_s3:           return "s3";
  case vctrs_type_scalar:       return "scalar";
  }
  never_reached("vec_type_as_str");
}


void vctrs_init_type_info(SEXP ns) {
  syms_vec_is_vector_dispatch = Rf_install("vec_is_vector");
  fns_vec_is_vector_dispatch = Rf_findVar(syms_vec_is_vector_dispatch, ns);
}
