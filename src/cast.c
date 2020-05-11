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
    out = vec_cast_dispatch(opts, x_type, to_type, &lossy);
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

static inline
SEXP vec_cast_default(SEXP x,
                      SEXP y,
                      SEXP x_arg,
                      SEXP to_arg,
                      enum df_fallback df_fallback) {
  SEXP df_fallback_obj = PROTECT(r_int(df_fallback));
  SEXP out = vctrs_eval_mask6(syms_vec_cast_default,
                              syms_x, x,
                              syms_to, y,
                              syms_x_arg, x_arg,
                              syms_to_arg, to_arg,
                              syms_from_dispatch, vctrs_shared_true,
                              syms_df_fallback, df_fallback_obj,
                              vctrs_ns_env);
  UNPROTECT(1);
  return out;
}

static
SEXP vec_cast_dispatch_s3(const struct cast_opts* opts) {
  SEXP x = opts->x;
  SEXP to = opts->to;
  SEXP x_arg_obj = PROTECT(vctrs_arg(opts->x_arg));
  SEXP to_arg_obj = PROTECT(vctrs_arg(opts->to_arg));

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
    SEXP out = vec_cast_default(x, to, x_arg_obj, to_arg_obj, opts->df_fallback);
    UNPROTECT(3);
    return out;
  }

  SEXP out = vctrs_dispatch4(method_sym, method,
                             syms_x, x,
                             syms_to, to,
                             syms_x_arg, x_arg_obj,
                             syms_to_arg, to_arg_obj);

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
SEXP vec_cast_common_params(SEXP xs, SEXP to, enum df_fallback df_fallback) {
  SEXP type = PROTECT(vec_ptype_common_params(xs,
                                              to,
                                              df_fallback,
                                              S3_FALLBACK_false));

  R_len_t n = Rf_length(xs);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    SET_VECTOR_ELT(out, i, vec_cast_params(elt, type,
                                           args_empty, args_empty,
                                           df_fallback));
  }

  SEXP names = PROTECT(Rf_getAttrib(xs, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(3);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_cast_common(SEXP xs, SEXP to) {
  return vec_cast_common_params(xs, to, DF_FALLBACK_DEFAULT);
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
SEXP vctrs_cast_common_params(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP dots = PROTECT(rlang_env_dots_list(env));
  SEXP to = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP df_fallback = PROTECT(Rf_eval(CAR(args), env));

  SEXP out = vec_cast_common_params(dots, to, r_int_get(df_fallback, 0));

  UNPROTECT(3);
  return out;
}


void vctrs_init_cast(SEXP ns) {
  syms_vec_cast_default = Rf_install("vec_default_cast");
}
