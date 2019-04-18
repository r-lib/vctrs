#include "vctrs.h"
#include "utils.h"

// Initialised at load time
static SEXP syms_vec_is_vector_dispatch = NULL;
static SEXP syms_vec_type_finalise_dispatch = NULL;
static SEXP fns_vec_is_vector_dispatch = NULL;
static SEXP fns_vec_type_finalise_dispatch = NULL;

// Defined below
static SEXP vec_type_impl(SEXP x, bool dispatch);


static SEXP vec_type_slice(SEXP x, SEXP empty) {
  if (ATTRIB(x) == R_NilValue) {
    return empty;
  } else {
    // Slicing preserves attributes
    return vec_slice(x, R_NilValue);
  }
}
static SEXP lgl_type(SEXP x) {
  if (vec_is_unspecified(x)) {
    return vctrs_shared_empty_uns;
  } else {
    return vec_type_slice(x, vctrs_shared_empty_lgl);
  }
}

static SEXP vec_type_impl(SEXP x, bool dispatch) {
  switch (vec_typeof_impl(x, dispatch)) {
  case vctrs_type_scalar:    return x;
  case vctrs_type_null:      return R_NilValue;
  case vctrs_type_logical:   return lgl_type(x);
  case vctrs_type_integer:   return vec_type_slice(x, vctrs_shared_empty_int);
  case vctrs_type_double:    return vec_type_slice(x, vctrs_shared_empty_dbl);
  case vctrs_type_complex:   return vec_type_slice(x, vctrs_shared_empty_cpl);
  case vctrs_type_character: return vec_type_slice(x, vctrs_shared_empty_chr);
  case vctrs_type_raw:       return vec_type_slice(x, vctrs_shared_empty_raw);
  case vctrs_type_list:      return vec_type_slice(x, vctrs_shared_empty_list);
  case vctrs_type_dataframe: return df_map(x, &vec_type);
  case vctrs_type_s3:        return with_proxy(x, &vec_type_impl, vctrs_shared_empty_int);
  }
}

// [[ include("vctrs.h"), register ]]
SEXP vec_type(SEXP x) {
  return vec_type_impl(x, true);
}


bool vec_is_partial(SEXP x) {
  return x == R_NilValue || Rf_inherits(x, "vctrs_partial");
}


static SEXP vctrs_type_common_impl(SEXP current, SEXP types, bool spliced);

static SEXP vctrs_type_common_type(SEXP current, SEXP elt, bool spliced) {
  // Don't call `rlang_is_splice_box()` if we're already looking at a
  // spliced list because it's expensive
  if (!spliced && rlang_is_splice_box(elt)) {
    return vctrs_type_common_impl(current, rlang_unbox(elt), true);
  } else {
    return vec_type(elt);
  }
}
static SEXP vctrs_type_common_impl(SEXP current, SEXP types, bool spliced) {
  R_len_t n = Rf_length(types);

  if (!n) {
    return R_NilValue;
  }

  current = PROTECT(current);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(types, i);

    if (elt == R_NilValue) {
      continue;
    }

    SEXP elt_type = PROTECT(vctrs_type_common_type(current, elt, spliced));
    current = vec_type2(current, elt_type);

    // Reprotect `current`
    UNPROTECT(2);
    PROTECT(current);
  }

  UNPROTECT(1);
  return current;
}

SEXP vctrs_ext2_type_common(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP ptype = PROTECT(Rf_eval(CAR(args), env));
  if (!vec_is_partial(ptype)) {
    UNPROTECT(1);
    return vec_type(ptype);
  }

  if (r_is_true(r_peek_option("vctrs.no_guessing"))) {
    Rf_errorcall(R_NilValue, "strict mode is activated; you must supply complete `.ptype`.");
  }

  SEXP types = PROTECT(rlang_env_dots_values(env));

  SEXP type = PROTECT(vctrs_type_common_impl(ptype, types, false));
  type = vec_type_finalise(type);

  UNPROTECT(3);
  return type;
}


SEXP vec_type_finalise_rec(SEXP x, bool dispatch) {
  if (OBJECT(x)) {
    if (vec_is_unspecified(x)) {
      SEXP out = PROTECT(Rf_allocVector(LGLSXP, Rf_length(x)));
      r_lgl_fill(out, NA_LOGICAL);
      UNPROTECT(1);
      return out;
    }
  }

  switch (vec_typeof_impl(x, dispatch)) {
  case vctrs_type_dataframe: return df_map(x, &vec_type_finalise);
  case vctrs_type_s3:        return vctrs_dispatch1(syms_vec_type_finalise_dispatch, fns_vec_type_finalise_dispatch,
                                                    syms_x, x);
  default:                   return x;
  }
}

// [[ include("vctrs.h"), register ]]
SEXP vec_type_finalise(SEXP x) {
  return vec_type_finalise_rec(x, true);
}


bool is_data_frame(SEXP x) {
  return Rf_inherits(x, "data.frame");
}

bool is_record(SEXP x) {
  return Rf_inherits(x, "vctrs_rcrd") || Rf_inherits(x, "POSIXlt");
}

enum vctrs_type vec_typeof_impl(SEXP x, bool dispatch) {
  switch (TYPEOF(x)) {
  case NILSXP: return vctrs_type_null;
  case LGLSXP: return dispatch && OBJECT(x) ? vctrs_type_s3 : vctrs_type_logical;
  case INTSXP: return dispatch && OBJECT(x) ? vctrs_type_s3 : vctrs_type_integer;
  case REALSXP: return dispatch && OBJECT(x) ? vctrs_type_s3 : vctrs_type_double;
  case CPLXSXP: return dispatch && OBJECT(x) ? vctrs_type_s3 : vctrs_type_complex;
  case STRSXP: return dispatch && OBJECT(x) ? vctrs_type_s3 : vctrs_type_character;
  case RAWSXP: return dispatch && OBJECT(x) ? vctrs_type_s3 : vctrs_type_raw;
  case VECSXP:
    if (!OBJECT(x)) {
      return vctrs_type_list;
    } else if (is_data_frame(x)) {
      return vctrs_type_dataframe;
    } else if (dispatch) {
      return vctrs_type_s3;
    } else {
      return vctrs_type_scalar;
    }
  default:
    return vctrs_type_scalar;
  }
}
enum vctrs_type vec_typeof(SEXP x) {
  return vec_typeof_impl(x, true);
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
  case vctrs_type_s3:        return "s3";
  case vctrs_type_scalar:    return "scalar";
  }
}

static bool vec_is_vector_rec(SEXP x, bool dispatch) {
  switch (vec_typeof_impl(x, dispatch)) {
  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list:
  case vctrs_type_dataframe:
    return true;

  case vctrs_type_s3: {
    SEXP proxy = PROTECT(vec_proxy(x));
    bool out = vec_is_vector_rec(proxy, false);
    UNPROTECT(1);
    return out;
  }

  default:
    return false;
  }
}

// [[ include("vctrs.h") ]]
bool vec_is_vector(SEXP x) {
  return vec_is_vector_rec(x, true);
}

// [[ register ]]
SEXP vctrs_is_vector(SEXP x, SEXP dispatch) {
  return Rf_ScalarLogical(vec_is_vector_rec(x, LOGICAL(dispatch)[0]));
}

void vctrs_stop_unsupported_type(enum vctrs_type type, const char* fn) {
  Rf_errorcall(R_NilValue,
               "Unsupported vctrs type `%s` in `%s`",
               vec_type_as_str(type),
               fn);
}

SEXP vctrs_typeof(SEXP x, SEXP dispatch) {
  return Rf_mkString(vec_type_as_str(vec_typeof_impl(x, LOGICAL(dispatch)[0])));
}


SEXP vctrs_shared_empty_lgl = NULL;
SEXP vctrs_shared_empty_int = NULL;
SEXP vctrs_shared_empty_dbl = NULL;
SEXP vctrs_shared_empty_cpl = NULL;
SEXP vctrs_shared_empty_chr = NULL;
SEXP vctrs_shared_empty_raw = NULL;
SEXP vctrs_shared_empty_list = NULL;

SEXP vctrs_shared_true = NULL;
SEXP vctrs_shared_false = NULL;

Rcomplex vctrs_shared_na_cpl;

void vctrs_init_types(SEXP ns) {
  syms_vec_is_vector_dispatch = Rf_install("vec_is_vector");
  fns_vec_is_vector_dispatch = Rf_findVar(syms_vec_is_vector_dispatch, ns);

  syms_vec_type_finalise_dispatch = Rf_install("vec_type_finalise_dispatch");
  fns_vec_type_finalise_dispatch = Rf_findVar(syms_vec_type_finalise_dispatch, ns);

  vctrs_shared_empty_lgl = Rf_allocVector(LGLSXP, 0);
  R_PreserveObject(vctrs_shared_empty_lgl);
  MARK_NOT_MUTABLE(vctrs_shared_empty_lgl);

  vctrs_shared_empty_int = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(vctrs_shared_empty_int);
  MARK_NOT_MUTABLE(vctrs_shared_empty_int);

  vctrs_shared_empty_dbl = Rf_allocVector(REALSXP, 0);
  R_PreserveObject(vctrs_shared_empty_dbl);
  MARK_NOT_MUTABLE(vctrs_shared_empty_dbl);

  vctrs_shared_empty_cpl = Rf_allocVector(CPLXSXP, 0);
  R_PreserveObject(vctrs_shared_empty_cpl);
  MARK_NOT_MUTABLE(vctrs_shared_empty_cpl);

  vctrs_shared_empty_chr = Rf_allocVector(STRSXP, 0);
  R_PreserveObject(vctrs_shared_empty_chr);
  MARK_NOT_MUTABLE(vctrs_shared_empty_chr);

  vctrs_shared_empty_raw = Rf_allocVector(RAWSXP, 0);
  R_PreserveObject(vctrs_shared_empty_raw);
  MARK_NOT_MUTABLE(vctrs_shared_empty_raw);

  vctrs_shared_empty_list = Rf_allocVector(VECSXP, 0);
  R_PreserveObject(vctrs_shared_empty_list);
  MARK_NOT_MUTABLE(vctrs_shared_empty_list);

  vctrs_shared_true = Rf_allocVector(LGLSXP, 1);
  R_PreserveObject(vctrs_shared_true);
  MARK_NOT_MUTABLE(vctrs_shared_true);
  LOGICAL(vctrs_shared_true)[0] = 1;

  vctrs_shared_false = Rf_allocVector(LGLSXP, 1);
  R_PreserveObject(vctrs_shared_false);
  MARK_NOT_MUTABLE(vctrs_shared_false);
  LOGICAL(vctrs_shared_false)[0] = 0;

  vctrs_shared_na_cpl.i = NA_REAL;
  vctrs_shared_na_cpl.r = NA_REAL;
}
