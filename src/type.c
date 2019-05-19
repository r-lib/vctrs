#include "vctrs.h"
#include "utils.h"
#include "arg-counter.h"

// Initialised at load time
static SEXP syms_vec_type_finalise_dispatch = NULL;
static SEXP fns_vec_type_finalise_dispatch = NULL;


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

// [[ include("vctrs.h") ]]
bool vec_is_partial(SEXP x) {
  return x == R_NilValue || Rf_inherits(x, "vctrs_partial");
}

static SEXP vctrs_type_common_impl(SEXP current,
                                   SEXP types,
                                   struct counters* counters,
                                   bool spliced);

static SEXP vctrs_type2_common(SEXP current,
                               SEXP next,
                               struct counters* counters) {
  next = PROTECT(vec_type(next));

  int left;
  current = vec_type2(current, next, counters->curr_arg, counters->next_arg, &left);

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_swap(counters);
  }

  UNPROTECT(1);
  return current;
}

static SEXP vctrs_type2_common_box(SEXP current,
                                   SEXP next,
                                   struct counters* counters) {

  init_next_box_counters(counters, r_names(next));
  struct counters* box_counters = counters->next_box_counters;

  current = vctrs_type_common_impl(current, next, box_counters, true);

  counters->curr_arg = box_counters->curr_arg;
  counters->next = box_counters->next;

  return current;
}

static SEXP vctrs_type_common_impl(SEXP current,
                                   SEXP types,
                                   struct counters* counters,
                                   bool spliced) {
  R_len_t n = Rf_length(types);

  for (R_len_t i = 0; i < n; ++i, counters_inc(counters)) {
    PROTECT(current);

    SEXP next = VECTOR_ELT(types, i);

    // Don't call `rlang_is_splice_box()` if we're already looking at a
    // spliced list because it's expensive
    if (spliced || !rlang_is_splice_box(next)) {
      current = vctrs_type2_common(current, next, counters);
    } else {
      next = PROTECT(rlang_unbox(next));
      current = vctrs_type2_common_box(current, next, counters);
      UNPROTECT(1);
    }

    UNPROTECT(1);
  }

  return current;
}

SEXP vctrs_type_common(SEXP call, SEXP op, SEXP args, SEXP env) {
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

  // Store the box counters here as they might outlive their frame
  struct counters next_box_counters;
  struct counters prev_box_counters;

  // Start with the `.ptype` argument
  struct vctrs_arg_wrapper ptype_arg = new_wrapper_arg(NULL, ".ptype");

  struct counters counters;
  init_counters(&counters,
                r_names(types),
                (struct vctrs_arg*) &ptype_arg,
                &prev_box_counters,
                &next_box_counters);
  int n_protect = PROTECT_COUNTERS(&counters);

  SEXP type = PROTECT(vctrs_type_common_impl(ptype, types, &counters, false));
  type = vec_type_finalise(type);

  UNPROTECT(3 + n_protect);
  return type;
}


// [[ include("vctrs.h"); register() ]]
SEXP vec_type_finalise(SEXP x) {
  if (OBJECT(x)) {
    if (vec_is_unspecified(x)) {
      SEXP out = PROTECT(Rf_allocVector(LGLSXP, Rf_length(x)));
      r_lgl_fill(out, NA_LOGICAL);
      UNPROTECT(1);
      return out;
    }
  }

  switch (vec_typeof(x)) {
  case vctrs_type_dataframe: return df_map(x, &vec_type_finalise);
  case vctrs_type_s3:        return vctrs_dispatch1(syms_vec_type_finalise_dispatch, fns_vec_type_finalise_dispatch,
                                                    syms_x, x);
  default:                   return x;
  }
}


void vctrs_init_type(SEXP ns) {
  syms_vec_type_finalise_dispatch = Rf_install("vec_type_finalise_dispatch");
  fns_vec_type_finalise_dispatch = Rf_findVar(syms_vec_type_finalise_dispatch, ns);
}
