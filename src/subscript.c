#include <rlang.h>
#include "vctrs.h"
#include "ptype2.h"
#include "subscript.h"
#include "utils.h"
#include "dim.h"

#include "decl/subscript-decl.h"


r_obj* vec_as_subscript_opts(r_obj* subscript,
                             const struct subscript_opts* opts,
                             ERR* err) {
  if (vec_dim_n(subscript) != 1) {
    *err = new_error_subscript_type(subscript, opts, fns_cnd_body_subscript_dim);
    return r_null;
  }

  r_keep_t subscript_pi;
  KEEP_HERE(subscript, &subscript_pi);

  r_obj* orig_names = KEEP(r_names(subscript));

  switch (r_typeof(subscript)) {
  case R_TYPE_null:
    if (opts->numeric == SUBSCRIPT_TYPE_ACTION_CAST) {
      subscript = vctrs_shared_empty_int;
    }
    break;
  case R_TYPE_symbol:
    if (opts->character == SUBSCRIPT_TYPE_ACTION_CAST) {
      subscript = rlang_sym_as_character(subscript);
    }
    break;
  default:
    break;
  }
  KEEP_AT(subscript, subscript_pi);

  if (!vec_is_vector(subscript)) {
    *err = new_error_subscript_type(subscript, opts, r_null);
    FREE(2);
    return r_null;
  }

  if (r_is_object(subscript)) {
    subscript = obj_cast_subscript(subscript, opts, err);
  } else if (r_typeof(subscript) == R_TYPE_double) {
    subscript = dbl_cast_subscript(subscript, opts, err);
  }
  KEEP_AT(subscript, subscript_pi);

  if (*err) {
    FREE(2);
    return r_null;
  }

  // Coerce unspecified vectors to integer only if logical indices are
  // not allowed
  if (opts->logical == SUBSCRIPT_TYPE_ACTION_ERROR && vec_is_unspecified(subscript)) {
    struct vctrs_arg* arg = opts->subscript_arg;
    if (opts->numeric == SUBSCRIPT_TYPE_ACTION_CAST) {
      subscript = vec_cast(subscript,
                           vctrs_shared_empty_int,
                           arg,
                           NULL,
                           r_lazy_null);
    } else {
      subscript = vec_cast(subscript,
                           vctrs_shared_empty_chr,
                           arg,
                           NULL,
                           r_lazy_null);
    }
  }
  KEEP_AT(subscript, subscript_pi);

  enum subscript_type_action action = SUBSCRIPT_TYPE_ACTION_ERROR;
  switch (r_typeof(subscript)) {
  case R_TYPE_logical: action = opts->logical; break;
  case R_TYPE_integer: action = opts->numeric; break;
  case R_TYPE_character: action = opts->character; break;
  default: break;
  }

  if (action == SUBSCRIPT_TYPE_ACTION_ERROR) {
    *err = new_error_subscript_type(subscript, opts, r_null);
    FREE(2);
    return r_null;
  }

  if (orig_names != r_null) {
    // FIXME: Handle names in cast methods
    subscript = r_clone_referenced(subscript);
    KEEP_AT(subscript, subscript_pi);
    r_attrib_poke_names(subscript, orig_names);
  }

  FREE(2);
  return subscript;
}

static
r_obj* obj_cast_subscript(r_obj* subscript,
                          const struct subscript_opts* opts,
                          ERR* err) {
  int dir = 0;

  struct ptype2_opts ptype2_opts = {
    .x = subscript,
    .y = r_null,
    .x_arg = opts->subscript_arg
  };
  struct cast_opts cast_opts = {
    .x = subscript,
    .to = r_null,
    .x_arg = opts->subscript_arg
  };

  ptype2_opts.y = cast_opts.to = vctrs_shared_empty_lgl;
  if (vec_is_coercible(&ptype2_opts, &dir)) {
    return vec_cast_opts(&cast_opts);
  }

  ptype2_opts.y = cast_opts.to = vctrs_shared_empty_int;
  if (vec_is_coercible(&ptype2_opts, &dir)) {
    return vec_cast_opts(&cast_opts);
  }

  ptype2_opts.y = cast_opts.to = vctrs_shared_empty_chr;
  if (vec_is_coercible(&ptype2_opts, &dir)) {
    return vec_cast_opts(&cast_opts);
  }

  *err = new_error_subscript_type(subscript, opts, r_null);
  return r_null;
}

static
r_obj* dbl_cast_subscript(r_obj* subscript,
                          const struct subscript_opts* opts,
                          ERR* err) {
  double* p = r_dbl_begin(subscript);
  r_ssize n = r_length(subscript);

  r_obj* out = KEEP(r_alloc_integer(n));
  int* out_p = r_int_begin(out);

  for (r_ssize i = 0; i < n; ++i) {
    double elt = p[i];

    // Generally `(int) nan` results in the correct `na_int` value,
    // but this is not guaranteed, so we have to explicitly check for it.
    // https://stackoverflow.com/questions/10366485/problems-casting-nan-floats-to-int
    if (isnan(elt)) {
      out_p[i] = r_globals.na_int;
      continue;
    }

    if (!isfinite(elt) || elt <= INT_MIN || elt > INT_MAX) {
      // Once we throw lazy errors from the cast method, we should
      // throw the error here as well
      FREE(1);
      return dbl_cast_subscript_fallback(subscript, opts, err);
    }

    int elt_int = (int) elt;

    if (elt != elt_int) {
      FREE(1);
      return dbl_cast_subscript_fallback(subscript, opts, err);
    }

    out_p[i] = elt_int;
  }

  FREE(1);
  return out;
}

static
r_obj* dbl_cast_subscript_fallback(r_obj* subscript,
                                   const struct subscript_opts* opts,
                                   ERR* err) {
  struct cast_opts cast_opts = {
    .x = subscript,
    .to = vctrs_shared_empty_int,
    opts->subscript_arg
  };
  r_obj* out = KEEP(vec_cast_e(&cast_opts, err));
  if (*err) {
    r_obj* err_obj = KEEP(*err);

    r_obj* body = KEEP(vctrs_eval_mask1(syms_new_dbl_cast_subscript_body,
                                        syms_lossy_err, err_obj));

    *err = new_error_subscript_type(subscript, opts, body);
    FREE(3);
    return r_null;
  }

  FREE(1);
  return out;
}


// FFI -----------------------------------------------------------------

// [[ register() ]]
r_obj* ffi_as_subscript(r_obj* subscript,
                        r_obj* logical,
                        r_obj* numeric,
                        r_obj* character,
                        r_obj* frame) {
  struct r_lazy arg_ = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_);

  struct r_lazy call = { .x = r_syms.call, .env = frame };

  struct subscript_opts opts = {
    .logical = parse_subscript_arg_type(logical, "logical"),
    .numeric = parse_subscript_arg_type(numeric, "numeric"),
    .character = parse_subscript_arg_type(character, "character"),
    .subscript_arg = &arg,
    .call = call
  };

  ERR err = NULL;
  r_obj* out = vec_as_subscript_opts(subscript, &opts, &err);
  KEEP2(out, err);

  out = r_result_get(out, err);

  FREE(2);
  return out;
}

// [[ register() ]]
r_obj* ffi_as_subscript_result(r_obj* subscript,
                               r_obj* logical,
                               r_obj* numeric,
                               r_obj* character,
                               r_obj* frame) {
  struct r_lazy arg_ = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_);

  struct r_lazy call = { .x = r_syms.call, .env = frame };

  struct subscript_opts opts = {
    .logical = parse_subscript_arg_type(logical, "logical"),
    .numeric = parse_subscript_arg_type(numeric, "numeric"),
    .character = parse_subscript_arg_type(character, "character"),
    .subscript_arg = &arg,
    .call = call
  };

  ERR err = NULL;
  r_obj* out = vec_as_subscript_opts(subscript, &opts, &err);
  KEEP2(out, err);

  out = r_result(out, err);

  FREE(2);
  return out;
}


// Arguments -------------------------------------------------------------------

static
void stop_subscript_arg_type(const char* kind) {
  r_abort("`%s` must be one of \"cast\" or \"error\".", kind);
}
static
enum subscript_type_action parse_subscript_arg_type(r_obj* x,
                                                    const char* kind) {
  if (r_typeof(x) != R_TYPE_character || r_length(x) == 0) {
    stop_subscript_arg_type(kind);
  }

  const char* str = CHAR(STRING_ELT(x, 0));

  if (!strcmp(str, "cast")) return SUBSCRIPT_TYPE_ACTION_CAST;
  if (!strcmp(str, "error")) return SUBSCRIPT_TYPE_ACTION_ERROR;
  stop_subscript_arg_type(kind);

  r_stop_unreached("parse_subscript_arg_type");
}


// Conditions ------------------------------------------------------------------

static
r_obj* new_error_subscript_type(r_obj* subscript,
                                const struct subscript_opts* opts,
                                r_obj* body) {
  r_obj* logical = subscript_type_action_chr(opts->logical);
  r_obj* numeric = subscript_type_action_chr(opts->numeric);
  r_obj* character = subscript_type_action_chr(opts->character);

  subscript = KEEP(expr_protect(subscript));
  r_obj* subscript_arg = KEEP(vctrs_arg(opts->subscript_arg));
  r_obj* ffi_call = r_lazy_eval_protect(opts->call);

  r_obj* syms[] = {
    syms_i,
    syms_subscript_arg,
    syms_subscript_action,
    syms_call,
    syms_logical,
    syms_numeric,
    syms_character,
    syms_body,
    NULL
  };
  r_obj* args[] = {
    subscript,
    subscript_arg,
    get_opts_action(opts),
    ffi_call,
    logical,
    numeric,
    character,
    body,
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_new_error_subscript_type, syms, args));
  r_obj* out = r_eval(call, vctrs_ns_env);

  FREE(3);
  return out;
}


// Init ----------------------------------------------------------------

void vctrs_init_subscript(r_obj* ns) {
  syms_new_error_subscript_type = r_sym("new_error_subscript_type");
  syms_new_dbl_cast_subscript_body = r_sym("new_cnd_bullets_subscript_lossy_cast");
  syms_lossy_err = r_sym("lossy_err");

  fns_cnd_body_subscript_dim = r_eval(r_sym("cnd_body_subscript_dim"), ns);
}

static
r_obj* fns_cnd_body_subscript_dim = NULL;

static
r_obj* syms_new_dbl_cast_subscript_body = NULL;

static
r_obj* syms_lossy_err = NULL;

static
r_obj* syms_new_error_subscript_type = NULL;
