#include <rlang.h>
#include "vctrs.h"
#include "arg-counter.h"
#include "ptype-common.h"
#include "ptype2.h"
#include "type-data-frame.h"
#include "utils.h"
#include "assert.h"
#include "decl/ptype-decl.h"

// Initialised at load time
static SEXP syms_vec_ptype = NULL;

static SEXP syms_vec_ptype_finalise_dispatch = NULL;
static SEXP fns_vec_ptype_finalise_dispatch = NULL;


static inline SEXP vec_ptype_slice(SEXP x, SEXP empty);
static SEXP s3_type(SEXP x, struct vctrs_arg* x_arg);
static SEXP df_ptype(SEXP x, bool bare);

// [[ register() ]]
SEXP vctrs_ptype(SEXP x, SEXP x_arg) {
  struct vctrs_arg x_arg_ = vec_as_arg(x_arg);
  return vec_ptype(x, &x_arg_);
}

static SEXP col_ptype(SEXP x);

// [[ include("vctrs.h") ]]
SEXP vec_ptype(SEXP x, struct vctrs_arg* x_arg) {
  switch (vec_typeof(x)) {
  case vctrs_type_null:        return R_NilValue;
  case vctrs_type_unspecified: return vctrs_shared_empty_uns;
  case vctrs_type_logical:     return vec_ptype_slice(x, vctrs_shared_empty_lgl);
  case vctrs_type_integer:     return vec_ptype_slice(x, vctrs_shared_empty_int);
  case vctrs_type_double:      return vec_ptype_slice(x, vctrs_shared_empty_dbl);
  case vctrs_type_complex:     return vec_ptype_slice(x, vctrs_shared_empty_cpl);
  case vctrs_type_character:   return vec_ptype_slice(x, vctrs_shared_empty_chr);
  case vctrs_type_raw:         return vec_ptype_slice(x, vctrs_shared_empty_raw);
  case vctrs_type_list:        return vec_ptype_slice(x, vctrs_shared_empty_list);
  case vctrs_type_dataframe:   return df_ptype(x, true);
  case vctrs_type_s3:          return s3_type(x, x_arg);
  case vctrs_type_scalar:      stop_scalar_type(x, x_arg);
  }
  never_reached("vec_ptype");
}

static SEXP col_ptype(SEXP x) {
  return vec_ptype(x, args_empty);
}

static inline SEXP vec_ptype_slice(SEXP x, SEXP empty) {
  if (ATTRIB(x) == R_NilValue) {
    return empty;
  } else {
    // Slicing preserves attributes
    return vec_slice(x, R_NilValue);
  }
}

static SEXP s3_type(SEXP x, struct vctrs_arg* x_arg) {
  switch (class_type(x)) {
  case vctrs_class_bare_tibble:
    return df_ptype(x, true);

  case vctrs_class_data_frame:
    return df_ptype(x, false);

  case vctrs_class_bare_data_frame:
    r_stop_internal("s3_type", "Bare data frames should be handled by `vec_ptype()`.");

  case vctrs_class_none:
    r_stop_internal("s3_type", "Non-S3 classes should be handled by `vec_ptype()`.");

  default:
    break;
  }

  if (vec_is_partial(x)) {
    return x;
  }

  SEXP method = PROTECT(vec_ptype_method(x));

  SEXP out;

  if (method == r_null) {
    vec_assert_vector(x, x_arg);
    out = vec_slice(x, r_null);
  } else {
    out = vec_ptype_invoke(x, method);
  }

  UNPROTECT(1);
  return out;
}

static inline
SEXP vec_ptype_method(SEXP x) {
  SEXP cls = PROTECT(s3_get_class(x));
  SEXP method = s3_class_find_method("vec_ptype", cls, vctrs_method_table);
  UNPROTECT(1);
  return method;
}

static inline
SEXP vec_ptype_invoke(SEXP x, SEXP method) {
  return vctrs_dispatch1(syms_vec_ptype, method, syms_x, x);
}

SEXP df_ptype(SEXP x, bool bare) {
  SEXP row_nms = PROTECT(df_rownames(x));

  SEXP ptype = R_NilValue;
  if (bare) {
    ptype = PROTECT(bare_df_map(x, &col_ptype));
  } else {
    ptype = PROTECT(df_map(x, &col_ptype));
  }

  if (TYPEOF(row_nms) == STRSXP) {
    Rf_setAttrib(ptype, R_RowNamesSymbol, vctrs_shared_empty_chr);
  }

  UNPROTECT(2);
  return ptype;
}

static SEXP vec_ptype_finalise_unspecified(SEXP x);
static SEXP vec_ptype_finalise_dispatch(SEXP x);

// [[ include("vctrs.h"); register() ]]
SEXP vec_ptype_finalise(SEXP x) {
  if (x == R_NilValue) {
    return x;
  }

  if (!OBJECT(x)) {
    vec_assert_vector(x, args_empty);
    return x;
  }

  if (vec_is_unspecified(x)) {
    return vec_ptype_finalise_unspecified(x);
  }

  if (vec_is_partial(x)) {
    return vec_ptype_finalise_dispatch(x);
  }

  vec_assert_vector(x, args_empty);

  switch (class_type(x)) {
  case vctrs_class_bare_tibble:
  case vctrs_class_bare_data_frame:
    return bare_df_map(x, &vec_ptype_finalise);

  case vctrs_class_data_frame:
    x = PROTECT(df_map(x, &vec_ptype_finalise));

    if (Rf_inherits(x, "vctrs:::df_fallback")) {
      SEXP seen_tibble_attr = PROTECT(Rf_getAttrib(x, Rf_install("seen_tibble")));
      bool seen_tibble = r_is_true(seen_tibble_attr);
      UNPROTECT(1);

      if (seen_tibble) {
        r_poke_class(x, classes_tibble);
      } else {
        r_poke_class(x, classes_data_frame);
      }

      Rf_setAttrib(x, Rf_install("known_classes"), R_NilValue);
      Rf_setAttrib(x, Rf_install("seen_tibble"), R_NilValue);
    }

    UNPROTECT(1);
    return x;

  case vctrs_class_none:
    r_stop_internal("vec_ptype_finalise", "Non-S3 classes should have returned by now.");

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

static SEXP vctrs_type2_common(SEXP current, SEXP next, struct counters* counters, void* data);

// [[ register(external = TRUE) ]]
SEXP vctrs_type_common(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP types = PROTECT(rlang_env_dots_values(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env));

  SEXP out = vec_ptype_common_params(types, ptype, DF_FALLBACK_DEFAULT, S3_FALLBACK_false);

  UNPROTECT(2);
  return out;
}

// [[ register(external = TRUE) ]]
SEXP vctrs_ptype_common_opts(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP types = PROTECT(rlang_env_dots_values(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP opts = PROTECT(Rf_eval(CAR(args), env));

  const struct fallback_opts c_opts = new_fallback_opts(opts);

  SEXP out = vec_ptype_common_opts(types, ptype, &c_opts);

  UNPROTECT(3);
  return out;
}
// [[ include("ptype-common.h") ]]
SEXP vec_ptype_common_params(SEXP dots,
                             SEXP ptype,
                             enum df_fallback df_fallback,
                             enum s3_fallback s3_fallback) {
  struct fallback_opts opts = {
    .df = df_fallback,
    .s3 = s3_fallback
  };

  return vec_ptype_common_opts(dots, ptype, &opts);
}

// [[ include("ptype-common.h") ]]
SEXP vec_ptype_common_opts(SEXP dots,
                           SEXP ptype,
                           const struct fallback_opts* opts) {
  if (!vec_is_partial(ptype)) {
    return vec_ptype(ptype, args_dot_ptype);
  }

  if (r_is_true(r_peek_option("vctrs.no_guessing"))) {
    Rf_errorcall(R_NilValue, "strict mode is activated; you must supply complete `.ptype`.");
  }

  // Remove constness
  struct fallback_opts mut_opts = *opts;

  // Start reduction with the `.ptype` argument
  SEXP type = PROTECT(reduce(ptype, args_dot_ptype, dots, &vctrs_type2_common, &mut_opts));
  type = vec_ptype_finalise(type);

  UNPROTECT(1);
  return type;
}


static SEXP vctrs_type2_common(SEXP current,
                               SEXP next,
                               struct counters* counters,
                               void* data) {
  int left = -1;

  const struct ptype2_opts opts = {
    .x = current,
    .y = next,
    .x_arg = counters->curr_arg,
    .y_arg = counters->next_arg,
    .fallback = *((struct fallback_opts*) data)
  };

  current = vec_ptype2_opts(&opts, &left);

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_shift(counters);
  }

  return current;
}


void vctrs_init_type(SEXP ns) {
  syms_vec_ptype = Rf_install("vec_ptype");

  syms_vec_ptype_finalise_dispatch = Rf_install("vec_ptype_finalise_dispatch");
  fns_vec_ptype_finalise_dispatch = Rf_findVar(syms_vec_ptype_finalise_dispatch, ns);
}
