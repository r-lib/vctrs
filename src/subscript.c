#include "vctrs.h"
#include "subscript.h"
#include "utils.h"

static SEXP new_error_subscript_type(SEXP subscript,
                                     const struct vec_as_subscript_opts* opts,
                                     SEXP body,
                                     SEXP parent);
static enum subscript_type_action parse_subscript_arg_type(SEXP x, const char* kind);

static SEXP obj_cast_subscript(SEXP subscript,
                               const struct vec_as_subscript_opts* opts,
                               ERR* err);
static SEXP dbl_cast_subscript(SEXP subscript,
                               const struct vec_as_subscript_opts* opts,
                               ERR* err);


SEXP vec_as_subscript_opts(SEXP subscript,
                           const struct vec_as_subscript_opts* opts,
                           ERR* err) {
  PROTECT_INDEX subscript_pi;
  PROTECT_WITH_INDEX(subscript, &subscript_pi);

  SEXP orig_names = PROTECT(r_names(subscript));

  switch (TYPEOF(subscript)) {
  case NILSXP:
    if (opts->numeric == SUBSCRIPT_TYPE_ACTION_CAST) {
      subscript = vctrs_shared_empty_int;
    }
    break;
  case SYMSXP:
    if (opts->character == SUBSCRIPT_TYPE_ACTION_CAST) {
      subscript = rlang_sym_as_character(subscript);
    }
    break;
  default:
    break;
  }
  REPROTECT(subscript, subscript_pi);

  if (!vec_is_vector(subscript)) {
    *err = new_error_subscript_type(subscript, opts, R_NilValue, R_NilValue);
    UNPROTECT(2);
    return R_NilValue;
  }

  if (OBJECT(subscript)) {
    subscript = obj_cast_subscript(subscript, opts, err);
  } else if (TYPEOF(subscript) == REALSXP) {
    subscript = dbl_cast_subscript(subscript, opts, err);
  }
  REPROTECT(subscript, subscript_pi);

  if (*err) {
    UNPROTECT(2);
    return R_NilValue;
  }

  // Coerce unspecified vectors to integer only if logical indices are
  // not allowed
  if (opts->logical == SUBSCRIPT_TYPE_ACTION_ERROR && vec_is_unspecified(subscript)) {
    struct vctrs_arg* arg = opts->subscript_arg;
    if (opts->numeric == SUBSCRIPT_TYPE_ACTION_CAST) {
      subscript = vec_cast(subscript, vctrs_shared_empty_int, arg, NULL);
    } else {
      subscript = vec_cast(subscript, vctrs_shared_empty_chr, arg, NULL);
    }
  }
  REPROTECT(subscript, subscript_pi);

  enum subscript_type_action action = SUBSCRIPT_TYPE_ACTION_ERROR;
  switch (TYPEOF(subscript)) {
  case LGLSXP: action = opts->logical; break;
  case INTSXP: action = opts->numeric; break;
  case STRSXP: action = opts->character; break;
  default: break;
  }

  if (action == SUBSCRIPT_TYPE_ACTION_ERROR) {
    *err = new_error_subscript_type(subscript, opts, R_NilValue, R_NilValue);
    UNPROTECT(2);
    return R_NilValue;
  }

  // FIXME: Handle names in cast methods
  subscript = r_maybe_duplicate(subscript);
  REPROTECT(subscript, subscript_pi);
  r_poke_names(subscript, orig_names);

  UNPROTECT(2);
  return subscript;
}

static SEXP obj_cast_subscript(SEXP subscript,
                               const struct vec_as_subscript_opts* opts,
                               ERR* err) {
  int dir = 0;
  struct vctrs_arg* arg = opts->subscript_arg;

  // FIXME: No need for is-coercible check once vec-cast is restricted
  // to coercible casts

  if (vec_is_coercible(subscript, vctrs_shared_empty_lgl, arg, NULL, &dir)) {
    return vec_cast(subscript, vctrs_shared_empty_lgl, arg, NULL);
  }

  if (vec_is_coercible(subscript, vctrs_shared_empty_int, arg, NULL, &dir)) {
    return vec_cast(subscript, vctrs_shared_empty_int, arg, NULL);
  }

  if (vec_is_coercible(subscript, vctrs_shared_empty_chr, arg, NULL, &dir)) {
    return vec_cast(subscript, vctrs_shared_empty_chr, arg, NULL);
  }

  *err = new_error_subscript_type(subscript, opts, R_NilValue, R_NilValue);
  return R_NilValue;
}

static SEXP dbl_cast_subscript_fallback(SEXP subscript,
                                        const struct vec_as_subscript_opts* opts,
                                        ERR* err);
static SEXP dbl_cast_subscript_body = NULL;

static SEXP dbl_cast_subscript(SEXP subscript,
                               const struct vec_as_subscript_opts* opts,
                               ERR* err) {
  double* p = REAL(subscript);
  R_len_t n = Rf_length(subscript);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int* out_p = INTEGER(out);

  for (R_len_t i = 0; i < n; ++i) {
    double elt = p[i];
    int out_elt = (int) elt;

    // Detect out-of-bounds and fractional numbers
    if (elt > INT_MAX || out_elt != elt) {
      // Once we throw lazy errors from the cast method, we should
      // throw the error here as well
      UNPROTECT(1);
      return dbl_cast_subscript_fallback(subscript, opts, err);
    }

    out_p[i] = out_elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_cast_subscript_fallback(SEXP subscript,
                                        const struct vec_as_subscript_opts* opts,
                                        ERR* err) {

  SEXP out = PROTECT(vec_coercible_cast_e(subscript,
                                          vctrs_shared_empty_int,
                                          opts->subscript_arg,
                                          NULL,
                                          err));
  if (*err) {
    SEXP err_obj = PROTECT(*err);
    *err = new_error_subscript_type(subscript,
                                    opts,
                                    dbl_cast_subscript_body,
                                    err_obj);
    UNPROTECT(2);
    return R_NilValue;
  }

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_as_subscript_result(SEXP subscript,
                               SEXP logical,
                               SEXP numeric,
                               SEXP character,
                               SEXP arg_) {
  struct vctrs_arg arg = vec_as_arg(arg_);

  struct vec_as_subscript_opts opts = {
    .logical = parse_subscript_arg_type(logical, "logical"),
    .numeric = parse_subscript_arg_type(numeric, "numeric"),
    .character = parse_subscript_arg_type(character, "character"),
    .subscript_arg = &arg
  };

  ERR err = NULL;
  SEXP out = vec_as_subscript_opts(subscript, &opts, &err);
  PROTECT2(out, err);

  out = r_result(out, err);

  UNPROTECT(2);
  return out;
}

// [[ register() ]]
SEXP vctrs_as_subscript(SEXP subscript,
                        SEXP logical,
                        SEXP numeric,
                        SEXP character,
                        SEXP arg_) {
  struct vctrs_arg arg = vec_as_arg(arg_);

  struct vec_as_subscript_opts opts = {
    .logical = parse_subscript_arg_type(logical, "logical"),
    .numeric = parse_subscript_arg_type(numeric, "numeric"),
    .character = parse_subscript_arg_type(character, "character"),
    .subscript_arg = &arg
  };

  ERR err = NULL;
  SEXP out = vec_as_subscript_opts(subscript, &opts, &err);
  PROTECT2(out, err);

  out = r_result_get(out, err);

  UNPROTECT(2);
  return out;
}


// Arguments -------------------------------------------------------------------

static void stop_subscript_arg_type(const char* kind) {
  Rf_errorcall(R_NilValue, "`%s` must be one of \"cast\" or \"error\".", kind);
}
static enum subscript_type_action parse_subscript_arg_type(SEXP x, const char* kind) {
  if (TYPEOF(x) != STRSXP || Rf_length(x) == 0) {
    stop_subscript_arg_type(kind);
  }

  const char* str = CHAR(STRING_ELT(x, 0));

  if (!strcmp(str, "cast")) return SUBSCRIPT_TYPE_ACTION_CAST;
  if (!strcmp(str, "error")) return SUBSCRIPT_TYPE_ACTION_ERROR;
  stop_subscript_arg_type(kind);

  never_reached("parse_subscript_arg_type");
}


// Conditions ------------------------------------------------------------------

static SEXP syms_new_error_subscript_type = NULL;

static SEXP new_error_subscript_type(SEXP subscript,
                                     const struct vec_as_subscript_opts* opts,
                                     SEXP body,
                                     SEXP parent) {
  SEXP logical = subscript_type_action_chr(opts->logical);
  SEXP numeric = subscript_type_action_chr(opts->numeric);
  SEXP character = subscript_type_action_chr(opts->character);

  subscript = PROTECT(expr_protect(subscript));
  SEXP subscript_arg = PROTECT(vctrs_arg(opts->subscript_arg));

  SEXP syms[8] = {
    syms_i,
    syms_subscript_arg,
    syms_logical,
    syms_numeric,
    syms_character,
    syms_body,
    syms_parent,
    NULL
  };
  SEXP args[8] = {
    subscript,
    subscript_arg,
    logical,
    numeric,
    character,
    body,
    parent,
    NULL
  };

  SEXP call = PROTECT(r_call(syms_new_error_subscript_type, syms, args));
  SEXP out = Rf_eval(call, vctrs_ns_env);

  UNPROTECT(3);
  return out;
}


void vctrs_init_subscript(SEXP ns) {
  syms_new_error_subscript_type = Rf_install("new_error_subscript_type");
  dbl_cast_subscript_body = r_env_get(ns, Rf_install("cnd_bullets_subscript_lossy_cast"));
}
