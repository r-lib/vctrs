#include <rlang.h>
#include "vctrs.h"
#include "cast.h"
#include "dim.h"
#include "ptype2.h"
#include "ptype-common.h"
#include "type-data-frame.h"
#include "utils.h"

static
SEXP vec_cast_switch_native(const struct cast_opts* opts,
                            enum vctrs_type x_type,
                            enum vctrs_type to_type,
                            bool* lossy);

static SEXP vec_cast_dispatch_s3(const struct cast_opts* opts);

// [[ register() ]]
SEXP vctrs_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg to_arg = vec_as_arg(to_arg_);

  return vec_cast(x, to, &x_arg, &to_arg);
}

// [[ include("cast.h") ]]
SEXP vec_cast_opts(const struct cast_opts* opts) {
  SEXP x = opts->x;
  SEXP to = opts->to;
  struct vctrs_arg* x_arg = opts->x_arg;
  struct vctrs_arg* to_arg = opts->to_arg;

  if (x == R_NilValue) {
    if (!vec_is_partial(to)) {
      vec_assert(to, to_arg);
    }
    return x;
  }
  if (to == R_NilValue) {
    if (!vec_is_partial(x)) {
      vec_assert(x, x_arg);
    }
    return x;
  }

  enum vctrs_type x_type = vec_typeof(x);
  enum vctrs_type to_type = vec_typeof(to);

  if (x_type == vctrs_type_unspecified) {
    return vec_init(to, vec_size(x));
  }

  if (x_type == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg);
  }
  if (to_type == vctrs_type_scalar) {
    stop_scalar_type(to, to_arg);
  }

  if (has_dim(x) || has_dim(to)) {
    return vec_cast_dispatch_s3(opts);
  }

  SEXP out = R_NilValue;
  bool lossy = false;

  if (to_type == vctrs_type_s3 || x_type == vctrs_type_s3) {
    out = vec_cast_dispatch_native(opts, x_type, to_type, &lossy);
  } else {
    out = vec_cast_switch_native(opts, x_type, to_type, &lossy);
  }

  if (lossy || out == R_NilValue) {
    return vec_cast_dispatch_s3(opts);
  } else {
    return out;
  }
}

static
SEXP vec_cast_switch_native(const struct cast_opts* opts,
                            enum vctrs_type x_type,
                            enum vctrs_type to_type,
                            bool* lossy) {
  SEXP x = opts->x;

  int dir = 0;
  enum vctrs_type2 type2 = vec_typeof2_impl(x_type, to_type, &dir);

  switch (type2) {

  case vctrs_type2_logical_logical:
  case vctrs_type2_double_double:
  case vctrs_type2_character_character:
  case vctrs_type2_integer_integer:
    return x;

  case vctrs_type2_logical_integer:
    if (dir == 0) {
      return lgl_as_integer(x, lossy);
    } else {
      return int_as_logical(x, lossy);
    }

  case vctrs_type2_logical_double:
    if (dir == 0) {
      return lgl_as_double(x, lossy);
    } else {
      return dbl_as_logical(x, lossy);
    }

  case vctrs_type2_integer_double:
    if (dir == 0) {
      return int_as_double(x, lossy);
    } else {
      return dbl_as_integer(x, lossy);
    }

  case vctrs_type2_dataframe_dataframe:
    return df_cast_opts(opts);

  default:
    break;
  }

  return R_NilValue;
}


static SEXP syms_vec_cast_default = NULL;

// [[ include("cast.h") ]]
SEXP vec_cast_default(SEXP x,
                      SEXP to,
                      SEXP x_arg,
                      SEXP to_arg,
                      const struct fallback_opts* opts) {
  SEXP df_fallback = PROTECT(r_int(opts->df));
  SEXP s3_fallback = PROTECT(r_int(opts->s3));
  SEXP out = vctrs_eval_mask7(syms_vec_cast_default,
                              syms_x, x,
                              syms_to, to,
                              syms_x_arg, x_arg,
                              syms_to_arg, to_arg,
                              syms_from_dispatch, vctrs_shared_true,
                              syms_df_fallback, df_fallback,
                              syms_s3_fallback, s3_fallback);
  UNPROTECT(2);
  return out;
}

static
SEXP vec_cast_dispatch_s3(const struct cast_opts* opts) {
  SEXP x = opts->x;
  SEXP to = opts->to;
  SEXP r_x_arg = PROTECT(vctrs_arg(opts->x_arg));
  SEXP r_to_arg = PROTECT(vctrs_arg(opts->to_arg));

  SEXP method_sym = R_NilValue;
  SEXP method = s3_find_method_xy("vec_cast", to, x, vctrs_method_table, &method_sym);

  // Compatibility with legacy double dispatch mechanism
  if (method == R_NilValue) {
    SEXP to_method_sym = R_NilValue;
    SEXP to_method = PROTECT(s3_find_method2("vec_cast",
                                             to,
                                             vctrs_method_table,
                                             &to_method_sym));

    if (to_method != R_NilValue) {
      const char* to_method_str = CHAR(PRINTNAME(to_method_sym));
      SEXP to_table = s3_get_table(CLOENV(to_method));

      method = s3_find_method2(to_method_str,
                               x,
                               to_table,
                               &method_sym);
    }

    UNPROTECT(1);
  }

  PROTECT(method);

  if (method == R_NilValue) {
    SEXP out = vec_cast_default(x, to, r_x_arg, r_to_arg, &(opts->fallback));
    UNPROTECT(3);
    return out;
  }

  SEXP out = vec_invoke_coerce_method(method_sym, method,
                                      syms_x, x,
                                      syms_to, to,
                                      syms_x_arg, r_x_arg,
                                      syms_to_arg, r_to_arg,
                                      &(opts->fallback));

  UNPROTECT(3);
  return out;
}

struct cast_err_data {
  const struct cast_opts* opts;
  SEXP out;
};

static void vec_cast_e_cb(void* data_) {
  struct cast_err_data* data = (struct cast_err_data*) data_;
  data->out = vec_cast_opts(data->opts);
}

// [[ include("cast.h") ]]
SEXP vec_cast_e(const struct cast_opts* opts,
                ERR* err) {
  struct cast_err_data data = {
    .opts = opts,
    .out = R_NilValue
  };

  *err = r_try_catch(&vec_cast_e_cb,
                     &data,
                     syms_vctrs_error_cast_lossy,
                     NULL,
                     NULL);
  return data.out;
}

// [[ include("cast.h") ]]
SEXP vec_cast_common_opts(SEXP xs,
                          SEXP to,
                          const struct fallback_opts* fallback_opts) {
  SEXP type = PROTECT(vec_ptype_common_opts(xs, to, fallback_opts));

  R_len_t n = Rf_length(xs);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    struct cast_opts opts = {
      .x = elt,
      .to = type,
      .fallback = *fallback_opts
    };
    SET_VECTOR_ELT(out, i, vec_cast_opts(&opts));
  }

  SEXP names = PROTECT(Rf_getAttrib(xs, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(3);
  return out;
}
// [[ include("cast.h") ]]
SEXP vec_cast_common_params(SEXP xs,
                            SEXP to,
                            enum df_fallback df_fallback,
                            enum s3_fallback s3_fallback) {
  struct fallback_opts opts = {
    .df = df_fallback,
    .s3 = s3_fallback
  };
  return vec_cast_common_opts(xs, to, &opts);
}

// [[ include("vctrs.h") ]]
SEXP vec_cast_common(SEXP xs, SEXP to) {
  return vec_cast_common_params(xs, to, DF_FALLBACK_DEFAULT, S3_FALLBACK_DEFAULT);
}

// [[ register(external = TRUE) ]]
SEXP vctrs_cast_common(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP dots = PROTECT(rlang_env_dots_list(env));
  SEXP to = PROTECT(Rf_eval(CAR(args), env));

  SEXP out = vec_cast_common(dots, to);

  UNPROTECT(2);
  return out;
}

// [[ register(external = TRUE) ]]
SEXP vctrs_cast_common_opts(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP dots = PROTECT(rlang_env_dots_list(env));
  SEXP to = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP opts = PROTECT(Rf_eval(CAR(args), env));

  const struct fallback_opts c_opts = new_fallback_opts(opts);

  SEXP out = vec_cast_common_opts(dots, to, &c_opts);

  UNPROTECT(3);
  return out;
}

// [[ include("cast.h") ]]
struct cast_opts new_cast_opts(SEXP x,
                               SEXP to,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* to_arg,
                               SEXP opts) {
  return (struct cast_opts) {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg,
    .fallback = {
      .df = r_int_get(r_list_get(opts, 0), 0),
      .s3 = r_int_get(r_list_get(opts, 1), 0)
    }
  };
}


void vctrs_init_cast(SEXP ns) {
  syms_vec_cast_default = Rf_install("vec_default_cast");
}
