#include "vctrs.h"
#include "utils.h"
#include "arg-counter.h"

// Initialised at load time
static SEXP syms_vec_ptype_finalise_dispatch = NULL;
static SEXP fns_vec_ptype_finalise_dispatch = NULL;


static SEXP vec_type_slice(SEXP x, SEXP empty);
static SEXP s3_type(SEXP x);

// [[ include("vctrs.h"); register() ]]
SEXP vec_type(SEXP x) {
  switch (vec_typeof(x)) {
  case vctrs_type_null:        return R_NilValue;
  case vctrs_type_unspecified: return vctrs_shared_empty_uns;
  case vctrs_type_logical:     return vec_type_slice(x, vctrs_shared_empty_lgl);
  case vctrs_type_integer:     return vec_type_slice(x, vctrs_shared_empty_int);
  case vctrs_type_double:      return vec_type_slice(x, vctrs_shared_empty_dbl);
  case vctrs_type_complex:     return vec_type_slice(x, vctrs_shared_empty_cpl);
  case vctrs_type_character:   return vec_type_slice(x, vctrs_shared_empty_chr);
  case vctrs_type_raw:         return vec_type_slice(x, vctrs_shared_empty_raw);
  case vctrs_type_list:        return vec_type_slice(x, vctrs_shared_empty_list);
  case vctrs_type_dataframe:   return bare_df_map(x, &vec_type);
  case vctrs_type_s3:          return s3_type(x);
  case vctrs_type_scalar:      stop_scalar_type(x, args_empty);
  }
  never_reached("vec_type_impl");
}

static SEXP vec_type_slice(SEXP x, SEXP empty) {
  if (ATTRIB(x) == R_NilValue) {
    return empty;
  } else {
    // Slicing preserves attributes
    return vec_slice(x, R_NilValue);
  }
}
static SEXP s3_type(SEXP x) {
  switch(class_type(x)) {
  case vctrs_class_bare_tibble:
    return bare_df_map(x, &vec_type);

  case vctrs_class_data_frame:
    return df_map(x, &vec_type);

  case vctrs_class_bare_data_frame:
    Rf_errorcall(R_NilValue, "Internal error: Bare data frames should be handled by `vec_type()`");

  case vctrs_class_none:
    Rf_errorcall(R_NilValue, "Internal error: Non-S3 classes should be handled by `vec_type()`");

  default:
    break;
  }

  if (vec_is_partial(x)) {
    return x;
  }

  vec_assert(x, args_empty);
  return vec_slice(x, R_NilValue);
}

static SEXP vec_ptype_finalise_unspecified(SEXP x);
static SEXP vec_ptype_finalise_dispatch(SEXP x);

// [[ include("vctrs.h"); register() ]]
SEXP vec_ptype_finalise(SEXP x) {
  if (x == R_NilValue) {
    return x;
  }

  if (!OBJECT(x)) {
    vec_assert(x, args_empty);
    return x;
  }

  if (vec_is_unspecified(x)) {
    return vec_ptype_finalise_unspecified(x);
  }

  if (vec_is_partial(x)) {
    return vec_ptype_finalise_dispatch(x);
  }

  vec_assert(x, args_empty);

  switch(class_type(x)) {
  case vctrs_class_bare_tibble:
  case vctrs_class_bare_data_frame:
    return bare_df_map(x, &vec_ptype_finalise);

  case vctrs_class_data_frame:
    return df_map(x, &vec_ptype_finalise);

  case vctrs_class_none:
    Rf_errorcall(R_NilValue, "Internal error: Non-S3 classes should have returned by now");

  default:
    return vec_ptype_finalise_dispatch(x);
  }
}

static SEXP vec_ptype_finalise_unspecified(SEXP x) {
  R_len_t size = Rf_length(x);

  if (size == 0) {
    return vctrs_shared_empty_lgl;
  }

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));
  r_lgl_fill(out, NA_LOGICAL, size);

  UNPROTECT(1);
  return out;
}

static SEXP vec_ptype_finalise_dispatch(SEXP x) {
  return vctrs_dispatch1(
    syms_vec_ptype_finalise_dispatch, fns_vec_ptype_finalise_dispatch,
    syms_x, x
  );
}


SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);
static SEXP vctrs_type2_common(SEXP current, SEXP next, struct counters* counters);

// [[ register(external = TRUE) ]]
SEXP vctrs_type_common(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP types = PROTECT(rlang_env_dots_values(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env));

  SEXP out = vctrs_type_common_impl(types, ptype);

  UNPROTECT(2);
  return out;
}

SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype) {
  if (!vec_is_partial(ptype)) {
    return vec_type(ptype);
  }

  if (r_is_true(r_peek_option("vctrs.no_guessing"))) {
    Rf_errorcall(R_NilValue, "strict mode is activated; you must supply complete `.ptype`.");
  }

  // Start reduction with the `.ptype` argument
  struct vctrs_arg ptype_arg = new_wrapper_arg(NULL, ".ptype");

  SEXP type = PROTECT(reduce(ptype, &ptype_arg, dots, &vctrs_type2_common));
  type = vec_ptype_finalise(type);

  UNPROTECT(1);
  return type;
}


static SEXP vctrs_type2_common(SEXP current, SEXP next, struct counters* counters) {
  int left = -1;
  current = vec_type2(current, next, counters->curr_arg, counters->next_arg, &left);

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_shift(counters);
  }

  return current;
}


void vctrs_init_type(SEXP ns) {
  syms_vec_ptype_finalise_dispatch = Rf_install("vec_ptype_finalise_dispatch");
  fns_vec_ptype_finalise_dispatch = Rf_findVar(syms_vec_ptype_finalise_dispatch, ns);
}
