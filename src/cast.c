#include "vctrs.h"
#include "cast.h"
#include "type-data-frame.h"
#include "utils.h"

static SEXP vec_cast_switch_native(SEXP x,
                                   SEXP to,
                                   enum vctrs_type x_type,
                                   enum vctrs_type to_type,
                                   struct vctrs_arg* x_arg,
                                   struct vctrs_arg* to_arg,
                                   bool* lossy);

static SEXP vec_cast_dispatch_s3(SEXP x,
                                 SEXP to,
                                 struct vctrs_arg* x_arg,
                                 struct vctrs_arg* to_arg);

// [[ register() ]]
SEXP vctrs_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg to_arg = vec_as_arg(to_arg_);

  return vec_cast(x, to, &x_arg, &to_arg);
}

// [[ include("vctrs.h") ]]
SEXP vec_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
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
    return vec_cast_dispatch_s3(x, to, x_arg, to_arg);
  }

  SEXP out = R_NilValue;
  bool lossy = false;

  if (to_type == vctrs_type_s3 || x_type == vctrs_type_s3) {
    out = vec_cast_dispatch(x, to, x_type, to_type, x_arg, to_arg, &lossy);
  } else {
    out = vec_cast_switch_native(x, to, x_type, to_type, x_arg, to_arg, &lossy);
  }

  if (lossy || out == R_NilValue) {
    return vec_cast_dispatch_s3(x, to, x_arg, to_arg);
  } else {
    return out;
  }
}

static SEXP vec_cast_switch_native(SEXP x,
                                   SEXP to,
                                   enum vctrs_type x_type,
                                   enum vctrs_type to_type,
                                   struct vctrs_arg* x_arg,
                                   struct vctrs_arg* to_arg,
                                   bool* lossy) {
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

  case vctrs_type2_logical_character:
    if (dir == 0) {
      return Rf_coerceVector(x, STRSXP);
    } else {
      return chr_as_logical(x, lossy);
    }

  case vctrs_type2_integer_character:
  case vctrs_type2_double_character:
    if (dir == 0) {
      return Rf_coerceVector(x, STRSXP);
    } else {
      // TODO: Implement with `R_strtod()` from R_ext/utils.h?
      break;
    }

  case vctrs_type2_dataframe_dataframe:
    return df_cast(x, to, x_arg, to_arg);

  default:
    break;
  }

  return R_NilValue;
}


static SEXP syms_vec_cast_default = NULL;

static inline SEXP vec_cast_default(SEXP x,
                                    SEXP y,
                                    SEXP x_arg,
                                    SEXP to_arg) {
  return vctrs_eval_mask4(syms_vec_cast_default,
                          syms_x, x,
                          syms_to, y,
                          syms_x_arg, x_arg,
                          syms_to_arg, to_arg,
                          vctrs_ns_env);
}

static SEXP vec_cast_dispatch_s3(SEXP x,
                                 SEXP to,
                                 struct vctrs_arg* x_arg,
                                 struct vctrs_arg* to_arg) {
  SEXP x_arg_obj = PROTECT(vctrs_arg(x_arg));
  SEXP to_arg_obj = PROTECT(vctrs_arg(to_arg));

  SEXP to_method_sym = R_NilValue;
  SEXP to_method = PROTECT(s3_find_method2("vec_cast",
                                           to,
                                           vctrs_method_table,
                                           &to_method_sym));

  if (to_method == R_NilValue) {
    SEXP out = vec_cast_default(x, to, x_arg_obj, to_arg_obj);
    UNPROTECT(3);
    return out;
  }

  const char* to_method_str = CHAR(PRINTNAME(to_method_sym));
  SEXP to_table = s3_get_table(CLOENV(to_method));

  SEXP x_method_sym = R_NilValue;
  SEXP x_method = PROTECT(s3_find_method2(to_method_str,
                                          x,
                                          to_table,
                                          &x_method_sym));

  if (x_method == R_NilValue) {
    SEXP out = vec_cast_default(x, to, x_arg_obj, to_arg_obj);
    UNPROTECT(4);
    return out;
  }

  SEXP out = vctrs_dispatch4(x_method_sym, x_method,
                             syms_x, x,
                             syms_to, to,
                             syms_x_arg, x_arg_obj,
                             syms_to_arg, to_arg_obj);

  UNPROTECT(4);
  return out;
}


struct vec_is_coercible_data {
  SEXP x;
  SEXP y;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* y_arg;
  int* dir;
};

static void vec_is_coercible_cb(void* data_) {
  struct vec_is_coercible_data* data = (struct vec_is_coercible_data*) data_;
  vec_ptype2(data->x, data->y, data->x_arg, data->y_arg, data->dir);
}

static void vec_is_coercible_e(SEXP x,
                               SEXP y,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* y_arg,
                               int* dir,
                               ERR* err) {
  struct vec_is_coercible_data data = {
    .x = x,
    .y = y,
    .x_arg = x_arg,
    .y_arg = y_arg,
    .dir = dir
  };

  *err = r_try_catch(&vec_is_coercible_cb,
                     &data,
                     syms_vctrs_error_incompatible_type,
                     NULL,
                     NULL);
}

// [[ include("vctrs.h") ]]
bool vec_is_coercible(SEXP x,
                      SEXP y,
                      struct vctrs_arg* x_arg,
                      struct vctrs_arg* y_arg,
                      int* dir) {
  ERR err = NULL;
  vec_is_coercible_e(x, y, x_arg, y_arg, dir, &err);
  return !err;
}

// [[ register() ]]
SEXP vctrs_is_coercible(SEXP x, SEXP y, SEXP x_arg_, SEXP y_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg y_arg = vec_as_arg(y_arg_);;

  int dir = 0;
  return r_lgl(vec_is_coercible(x, y, &x_arg, &y_arg, &dir));
}

// [[ include("vctrs.h") ]]
SEXP vec_coercible_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  // Called for the side effect of generating an error if there is no
  // common type
  int _left;
  vec_ptype2(x, to, x_arg, to_arg, &_left);

  return vec_cast(x, to, x_arg, to_arg);
}

struct vec_coercible_cast_e_data {
  SEXP x;
  SEXP to;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* to_arg;
  SEXP out;
};
static void vec_coercible_cast_e_cb(void* data_) {
  struct vec_coercible_cast_e_data* data = (struct vec_coercible_cast_e_data*) data_;
  data->out = vec_cast(data->x, data->to, data->x_arg, data->to_arg);
}

// [[ include("vctrs.h") ]]
SEXP vec_coercible_cast_e(SEXP x,
                          SEXP to,
                          struct vctrs_arg* x_arg,
                          struct vctrs_arg* to_arg,
                          ERR* err) {
  int dir;
  vec_is_coercible_e(x, to, x_arg, to_arg, &dir, err);
  if (*err) {
    return R_NilValue;
  }

  struct vec_coercible_cast_e_data data = {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg,
    .out = R_NilValue
  };

  *err = r_try_catch(&vec_coercible_cast_e_cb,
                     &data,
                     syms_vctrs_error_cast_lossy,
                     NULL,
                     NULL);
  return data.out;
}

// [[ register() ]]
SEXP vctrs_coercible_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg to_arg = vec_as_arg(to_arg_);

  return vec_coercible_cast(x, to, &x_arg, &to_arg);
}


SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);

// [[ include("vctrs.h") ]]
SEXP vec_cast_common(SEXP xs, SEXP to) {
  SEXP type = PROTECT(vctrs_type_common_impl(xs, to));

  R_len_t n = Rf_length(xs);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    SET_VECTOR_ELT(out, i, vec_cast(elt, type, args_empty, args_empty));
  }

  SEXP names = PROTECT(Rf_getAttrib(xs, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(3);
  return out;
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


void vctrs_init_cast(SEXP ns) {
  syms_vec_cast_default = Rf_install("vec_default_cast");
}
