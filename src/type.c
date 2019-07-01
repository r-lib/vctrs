#include "vctrs.h"
#include "utils.h"
#include "arg-counter.h"

// Initialised at load time
static SEXP syms_vec_type_finalise_dispatch = NULL;
static SEXP fns_vec_type_finalise_dispatch = NULL;


static SEXP vec_type_slice(SEXP x, SEXP empty);
static SEXP lgl_type(SEXP x);

// [[ include("vctrs.h"); register() ]]
SEXP vec_type(SEXP x) {
  switch (vec_typeof(x)) {
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
  case vctrs_type_s3: {
    if (vec_is_vector(x)) {
      return vec_slice(x, R_NilValue);
    } else {
      // FIXME: Only used for partial frames
      return x;
    }
  }}
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
static SEXP lgl_type(SEXP x) {
  if (vec_is_unspecified(x)) {
    return vctrs_shared_empty_uns;
  } else {
    return vec_type_slice(x, vctrs_shared_empty_lgl);
  }
}


// [[ include("vctrs.h"); register() ]]
SEXP vec_type_finalise(SEXP x) {
  if (x == R_NilValue) {
    return x;
  }

  if (OBJECT(x)) {
    if (vec_is_unspecified(x)) {
      R_len_t n = Rf_length(x);
      SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
      r_lgl_fill(out, NA_LOGICAL, n);
      UNPROTECT(1);
      return out;
    }
  }

  if (!vec_is_partial(x)) {
    vec_assert(x, args_empty);
  }

  switch (vec_typeof(x)) {
  case vctrs_type_dataframe: return df_map(x, &vec_type_finalise);
  case vctrs_type_s3:        return vctrs_dispatch1(syms_vec_type_finalise_dispatch, fns_vec_type_finalise_dispatch,
                                                    syms_x, x);
  default:                   return x;
  }
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
  type = vec_type_finalise(type);

  UNPROTECT(1);
  return type;
}


static SEXP vctrs_type2_common(SEXP current, SEXP next, struct counters* counters) {
  next = PROTECT(vec_type(next));

  int left;
  current = vec_type2(current, next, counters->curr_arg, counters->next_arg, &left);

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_shift(counters);
  }

  UNPROTECT(1);
  return current;
}


void vctrs_init_type(SEXP ns) {
  syms_vec_type_finalise_dispatch = Rf_install("vec_ptype_finalise_dispatch");
  fns_vec_type_finalise_dispatch = Rf_findVar(syms_vec_type_finalise_dispatch, ns);
}
