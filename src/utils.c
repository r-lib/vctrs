#include "vctrs-core.h"
#include "vctrs.h"
#include "type-data-frame.h"
#include "vec-bool.h"
#include <R_ext/Rdynload.h>

// Initialised at load time
SEXP vctrs_method_table = NULL;
SEXP base_method_table = NULL;
SEXP s4_c_method_table = NULL;

SEXP strings_tbl = NULL;
SEXP strings_tbl_df = NULL;
SEXP strings_data_frame = NULL;
SEXP strings_date = NULL;
SEXP strings_posixct = NULL;
SEXP strings_posixlt = NULL;
SEXP strings_posixt = NULL;
SEXP strings_factor = NULL;
SEXP strings_ordered = NULL;
SEXP strings_list = NULL;

SEXP classes_data_frame = NULL;
SEXP classes_factor = NULL;
SEXP classes_ordered = NULL;
SEXP classes_date = NULL;
SEXP classes_posixct = NULL;
SEXP classes_tibble = NULL;
SEXP classes_vctrs_group_rle = NULL;

static SEXP syms_as_data_frame2 = NULL;
static SEXP fns_as_data_frame2 = NULL;


static SEXP vctrs_eval_mask_n_impl(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args, SEXP env);

/**
 * Evaluate with masked arguments
 *
 * This takes two arrays of argument (`args`) and argument names
 * `syms`). The names should correspond to formal arguments of `fn`.
 * Elements of `args` are assigned to their corresponding name in
 * `syms` directly in the current environment, i.e. the environment of
 * the closure wrapping the `.Call()` invokation. Since masked
 * evaluation causes side effects and variable assignments in that
 * frame environment, the native code invokation must be tailing: no
 * further R code (including `on.exit()` expressions) should be
 * evaluated in that closure wrapper.
 *
 * A call to `fn` is constructed with the
 * CARs and TAGs assigned symmetrically to the elements of
 * `syms`. This way the arguments are masked by symbols corresponding
 * to the formal parameters.
 *
 * @param fn The function to call.
 * @param syms A null-terminated array of symbols. The arguments
 *   `args` are assigned to these symbols. The assignment occurs in a
 *   child of `env` and the dispatch call refers to these symbols.
 * @param args A null-terminated array of arguments passed to the method.
 * @param env The environment in which to evaluate.
 */
SEXP vctrs_eval_mask_n(SEXP fn, SEXP* syms, SEXP* args) {
  return vctrs_eval_mask_n_impl(R_NilValue, fn, syms, args, vctrs_ns_env);
}
SEXP vctrs_eval_mask1(SEXP fn,
                      SEXP x_sym, SEXP x) {
  SEXP syms[2] = { x_sym, NULL };
  SEXP args[2] = { x, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}
SEXP vctrs_eval_mask2(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP y_sym, SEXP y) {
  SEXP syms[3] = { x_sym, y_sym, NULL };
  SEXP args[3] = { x, y, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}
SEXP vctrs_eval_mask3(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP y_sym, SEXP y,
                      SEXP z_sym, SEXP z) {
  SEXP syms[4] = { x_sym, y_sym, z_sym, NULL };
  SEXP args[4] = { x, y, z, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}
SEXP vctrs_eval_mask4(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4) {
  SEXP syms[5] = { x1_sym, x2_sym, x3_sym, x4_sym, NULL };
  SEXP args[5] = { x1, x2, x3, x4, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}
SEXP vctrs_eval_mask5(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP x5_sym, SEXP x5) {
  SEXP syms[6] = { x1_sym, x2_sym, x3_sym, x4_sym, x5_sym, NULL };
  SEXP args[6] = { x1, x2, x3, x4, x5, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}
SEXP vctrs_eval_mask6(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP x5_sym, SEXP x5,
                      SEXP x6_sym, SEXP x6) {
  SEXP syms[7] = { x1_sym, x2_sym, x3_sym, x4_sym, x5_sym, x6_sym, NULL };
  SEXP args[7] = { x1, x2, x3, x4, x5, x6, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}
SEXP vctrs_eval_mask7(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP x5_sym, SEXP x5,
                      SEXP x6_sym, SEXP x6,
                      SEXP x7_sym, SEXP x7) {
  SEXP syms[8] = { x1_sym, x2_sym, x3_sym, x4_sym, x5_sym, x6_sym, x7_sym, NULL };
  SEXP args[8] = { x1, x2, x3, x4, x5, x6, x7, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}
r_obj* vctrs_eval_mask8(r_obj* fn,
                        r_obj* x1_sym, r_obj* x1,
                        r_obj* x2_sym, r_obj* x2,
                        r_obj* x3_sym, r_obj* x3,
                        r_obj* x4_sym, r_obj* x4,
                        r_obj* x5_sym, r_obj* x5,
                        r_obj* x6_sym, r_obj* x6,
                        r_obj* x7_sym, r_obj* x7,
                        r_obj* x8_sym, r_obj* x8) {
  r_obj* syms[9] = { x1_sym, x2_sym, x3_sym, x4_sym, x5_sym, x6_sym, x7_sym, x8_sym, NULL };
  r_obj* args[9] = { x1, x2, x3, x4, x5, x6, x7, x8, NULL };
  return vctrs_eval_mask_n(fn, syms, args);
}

/**
 * Dispatch in the current environment
 *
 * Like `vctrs_eval_mask_n()`, the arguments `args` are are assigned
 * to the symbols `syms`. In addition, the function `fn` is assigned
 * to `fn_sym`. The mask is the current environment which has hygiene
 * implications regarding the closure wrapping `.Call()`, as
 * documented in `vctrs_eval_mask_n()`.
 *
 * @param fn_sym A symbol to which `fn` is assigned.
 * @inheritParams vctrs_eval_mask_n
 */
SEXP vctrs_dispatch_n(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args) {
  SEXP mask = PROTECT(r_peek_frame());

  SEXP out = vctrs_eval_mask_n_impl(fn_sym, fn, syms, args, mask);

  UNPROTECT(1);
  return out;
}
SEXP vctrs_dispatch1(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x) {
  SEXP syms[2] = { x_sym, NULL };
  SEXP args[2] = { x, NULL };
  return vctrs_dispatch_n(fn_sym, fn, syms, args);
}
SEXP vctrs_dispatch2(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y) {
  SEXP syms[3] = { x_sym, y_sym, NULL };
  SEXP args[3] = { x, y, NULL };
  return vctrs_dispatch_n(fn_sym, fn, syms, args);
}
SEXP vctrs_dispatch3(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y,
                     SEXP z_sym, SEXP z) {
  SEXP syms[4] = { x_sym, y_sym, z_sym, NULL };
  SEXP args[4] = { x, y, z, NULL };
  return vctrs_dispatch_n(fn_sym, fn, syms, args);
}
SEXP vctrs_dispatch4(SEXP fn_sym, SEXP fn,
                     SEXP w_sym, SEXP w,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y,
                     SEXP z_sym, SEXP z) {
  SEXP syms[5] = { w_sym, x_sym, y_sym, z_sym, NULL };
  SEXP args[5] = { w, x, y, z, NULL };
  return vctrs_dispatch_n(fn_sym, fn, syms, args);
}
SEXP vctrs_dispatch6(SEXP fn_sym, SEXP fn,
                     SEXP x1_sym, SEXP x1,
                     SEXP x2_sym, SEXP x2,
                     SEXP x3_sym, SEXP x3,
                     SEXP x4_sym, SEXP x4,
                     SEXP x5_sym, SEXP x5,
                     SEXP x6_sym, SEXP x6) {
  SEXP syms[7] = { x1_sym, x2_sym, x3_sym, x4_sym, x5_sym, x6_sym, NULL };
  SEXP args[7] = { x1, x2, x3, x4, x5, x6, NULL };
  return vctrs_dispatch_n(fn_sym, fn, syms, args);
}

static SEXP vctrs_eval_mask_n_impl(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args, SEXP env) {
  SEXP mask = PROTECT(r_alloc_empty_environment(env));

  if (fn_sym != R_NilValue) {
    Rf_defineVar(fn_sym, fn, mask);
    fn = fn_sym;
  }

  SEXP body = PROTECT(r_call_n(fn, syms, syms));
  SEXP call_fn = PROTECT(r_new_function(R_NilValue, body, mask));
  SEXP call = PROTECT(Rf_lang1(call_fn));

  while (*syms) {
    Rf_defineVar(*syms, *args, mask);
    ++syms; ++args;
  }

  SEXP out = Rf_eval(call, env);

  UNPROTECT(4);
  return out;
}

// [[ register() ]]
SEXP vctrs_maybe_shared_col(SEXP x, SEXP i) {
  int i_ = r_int_get(i, 0) - 1;
  SEXP col = VECTOR_ELT(x, i_);
  bool out = MAYBE_SHARED(col);
  return Rf_ScalarLogical(out);
}

// [[ register() ]]
SEXP vctrs_new_df_unshared_col(void) {
  SEXP col = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(col)[0] = 1;

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));

  // In R 4.0.0, `SET_VECTOR_ELT()` bumps the REFCNT of
  // `col`. Because of this, `col` is now referenced (refcnt > 0),
  // but it isn't shared (refcnt > 1).
  SET_VECTOR_ELT(out, 0, col);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(names, 0, Rf_mkChar("x"));

  Rf_setAttrib(out, R_NamesSymbol, names);

  init_data_frame(out, 1);

  UNPROTECT(3);
  return out;
}

// [[ include("utils.h") ]]
SEXP map(SEXP x, SEXP (*fn)(SEXP)) {
  R_len_t n = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    SET_VECTOR_ELT(out, i, fn(VECTOR_ELT(x, i)));
  }

  SEXP nms = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  UNPROTECT(2);
  return out;
}
// [[ include("utils.h") ]]
SEXP map_with_data(SEXP x, SEXP (*fn)(SEXP, void*), void* data) {
  R_len_t n = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    SET_VECTOR_ELT(out, i, fn(VECTOR_ELT(x, i), data));
  }

  SEXP nms = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  UNPROTECT(2);
  return out;
}

// [[ include("utils.h") ]]
SEXP bare_df_map(SEXP df, SEXP (*fn)(SEXP)) {
  SEXP out = PROTECT(map(df, fn));

  // Shallow ownership over `out` because `map()` generates a fresh
  // list. We only care about "restoring" that bare list to the type of `df`,
  // not the columns, so not recursive.
  struct vec_restore_opts opts = {
    .ownership = VCTRS_OWNERSHIP_shallow,
    .recursively_proxied = false
  };

  out = vec_bare_df_restore(out, df, &opts);

  UNPROTECT(1);
  return out;
}

// [[ include("utils.h") ]]
SEXP df_map(SEXP df, SEXP (*fn)(SEXP)) {
  SEXP out = PROTECT(map(df, fn));

  // Shallow ownership over `out` because `map()` generates a fresh
  // list. We only care about "restoring" that bare list to the type of `df`,
  // not the contents, so not recursive.
  struct vec_restore_opts opts = {
    .ownership = VCTRS_OWNERSHIP_shallow,
    .recursively_proxied = false
  };

  out = vec_df_restore(out, df, &opts);

  UNPROTECT(1);
  return out;
}

#define RESIZE(CONST_DEREF, DEREF, CTYPE, SEXPTYPE) do {       \
  if (x_size == size) {                                        \
    return x;                                                  \
  }                                                            \
                                                               \
  const CTYPE* p_x = CONST_DEREF(x);                           \
                                                               \
  SEXP out = PROTECT(Rf_allocVector(SEXPTYPE, size));          \
  CTYPE* p_out = DEREF(out);                                   \
                                                               \
  r_ssize copy_size = (size > x_size) ? x_size : size;         \
                                                               \
  memcpy(p_out, p_x, copy_size * sizeof(CTYPE));               \
                                                               \
  UNPROTECT(1);                                                \
  return out;                                                  \
} while (0)

#define RESIZE_BARRIER(CONST_DEREF, SEXPTYPE, SET) do {        \
  if (x_size == size) {                                        \
    return x;                                                  \
  }                                                            \
                                                               \
  const SEXP* p_x = CONST_DEREF(x);                            \
                                                               \
  SEXP out = PROTECT(Rf_allocVector(SEXPTYPE, size));          \
                                                               \
  r_ssize copy_size = (size > x_size) ? x_size : size;         \
                                                               \
  for (r_ssize i = 0; i < copy_size; ++i) {                    \
    SET(out, i, p_x[i]);                                       \
  }                                                            \
                                                               \
  UNPROTECT(1);                                                \
  return out;                                                  \
} while (0)

// Faster than `Rf_xlengthgets()` because that fills the new extended
// locations with `NA`, which we don't need.
// [[ include("utils.h") ]]
SEXP int_resize(SEXP x, r_ssize x_size, r_ssize size) {
  RESIZE(INTEGER_RO, INTEGER, int, INTSXP);
}
// [[ include("utils.h") ]]
SEXP raw_resize(SEXP x, r_ssize x_size, r_ssize size) {
  RESIZE(RAW_RO, RAW, Rbyte, RAWSXP);
}
// [[ include("utils.h") ]]
SEXP chr_resize(SEXP x, r_ssize x_size, r_ssize size) {
  RESIZE_BARRIER(STRING_PTR_RO, STRSXP, SET_STRING_ELT);
}

#undef RESIZE
#undef RESIZE_BARRIER


inline void never_reached(const char* fn) {
  Rf_error("Internal error in `%s()`: Reached the unreachable.", fn);
}


static char s3_buf[200];

SEXP s3_paste_method_sym(const char* generic, const char* cls) {
  int gen_len = strlen(generic);
  int cls_len = strlen(cls);
  int dot_len = 1;
  if (gen_len + cls_len + dot_len >= sizeof(s3_buf)) {
    r_stop_internal("Generic or class name is too long.");
  }

  char* buf = s3_buf;

  memcpy(buf, generic, gen_len); buf += gen_len;
  *buf = '.'; ++buf;
  memcpy(buf, cls, cls_len); buf += cls_len;
  *buf = '\0';

  return Rf_install(s3_buf);
}

// First check in global env, then in method table
SEXP s3_get_method(const char* generic, const char* cls, SEXP table) {
  SEXP sym = s3_paste_method_sym(generic, cls);
  return s3_sym_get_method(sym, table);
}
SEXP s3_sym_get_method(SEXP sym, SEXP table) {
  // `r_env_get()` errors on missing bindings,
  // so we have to check with `r_env_has()`

  if (r_env_has(R_GlobalEnv, sym)) {
    SEXP method = r_env_get(R_GlobalEnv, sym);
    if (r_is_function(method)) {
      return method;
    }
  }

  if (r_env_has(table, sym)) {
    SEXP method = r_env_get(table, sym);
    if (r_is_function(method)) {
      return method;
    }
  }

  return R_NilValue;
}

// [[ register() ]]
SEXP vctrs_s3_find_method(SEXP generic, SEXP x, SEXP table) {
  return s3_find_method(r_chr_get_c_string(generic, 0), x, table);
}

// [[ register() ]]
r_obj* ffi_s3_get_method(r_obj* generic, r_obj* cls, r_obj* table) {
  if (!r_is_string(generic)) {
    r_stop_internal("`generic` must be a string");
  }
  if (!r_is_string(cls)) {
    r_stop_internal("`cls` must be a string");
  }
  return s3_get_method(r_chr_get_c_string(generic, 0),
                       r_chr_get_c_string(cls, 0),
                       table);
}

// [[ include("utils.h") ]]
SEXP s3_find_method(const char* generic, SEXP x, SEXP table) {
  if (!OBJECT(x)) {
    return R_NilValue;
  }

  SEXP cls = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  SEXP method = s3_class_find_method(generic, cls, table);

  UNPROTECT(1);
  return method;
}

// [[ include("utils.h") ]]
SEXP s3_class_find_method(const char* generic, SEXP cls, SEXP table) {
  // Avoid corrupt objects where `x` is an OBJECT(), but the class is NULL
  if (cls == R_NilValue) {
    return R_NilValue;
  }

  SEXP const* p_cls = STRING_PTR_RO(cls);
  int n_cls = Rf_length(cls);

  for (int i = 0; i < n_cls; ++i) {
    SEXP method = s3_get_method(generic, CHAR(p_cls[i]), table);
    if (method != R_NilValue) {
      return method;
    }
  }

  return R_NilValue;
}

// [[ include("utils.h") ]]
SEXP s3_get_class(SEXP x) {
  SEXP cls = R_NilValue;

  if (OBJECT(x)) {
    cls = Rf_getAttrib(x, R_ClassSymbol);
  }

  // This handles unclassed objects as well as gremlins objects where
  // `x` is an OBJECT(), but the class is NULL
  if (cls == R_NilValue) {
    cls = s3_bare_class(x);
  }

  if (!Rf_length(cls)) {
    r_stop_internal("Class must have length.");
  }

  return cls;
}

SEXP s3_get_class0(SEXP x) {
  SEXP cls = PROTECT(s3_get_class(x));
  SEXP out = STRING_ELT(cls, 0);
  UNPROTECT(1);
  return out;
}

// [[ include("utils.h") ]]
SEXP s3_find_method_xy(const char* generic,
                       SEXP x,
                       SEXP y,
                       SEXP table,
                       SEXP* method_sym_out) {
  SEXP x_class = PROTECT(s3_get_class0(x));
  SEXP y_class = PROTECT(s3_get_class0(y));

  SEXP method_sym = R_NilValue;
  method_sym = s3_paste_method_sym(generic, CHAR(x_class));
  method_sym = s3_paste_method_sym(CHAR(PRINTNAME(method_sym)), CHAR(y_class));

  SEXP method = s3_sym_get_method(method_sym, table);

  if (method == R_NilValue) {
    *method_sym_out = R_NilValue;
  } else {
    *method_sym_out = method_sym;
  }

  UNPROTECT(2);
  return method;
}

// [[ include("utils.h") ]]
SEXP s3_find_method2(const char* generic,
                     SEXP x,
                     SEXP table,
                     SEXP* method_sym_out) {
  SEXP cls = PROTECT(s3_get_class0(x));

  SEXP method_sym = s3_paste_method_sym(generic, CHAR(cls));
  SEXP method = s3_sym_get_method(method_sym, table);

  if (method == R_NilValue) {
    *method_sym_out = R_NilValue;
  } else {
    *method_sym_out = method_sym;
  }

  UNPROTECT(1);
  return method;
}


// [[ include("utils.h") ]]
SEXP s3_bare_class(SEXP x) {
  switch (TYPEOF(x)) {
  case NILSXP: return chrs_null;
  case LGLSXP: return chrs_logical;
  case INTSXP: return chrs_integer;
  case REALSXP: return chrs_double;
  case CPLXSXP: return chrs_complex;
  case STRSXP: return chrs_character;
  case RAWSXP: return chrs_raw;
  case VECSXP: return chrs_list;
  case EXPRSXP: return chrs_expression;
  case CLOSXP:
  case SPECIALSXP:
  case BUILTINSXP: return chrs_function;
  default: stop_unimplemented_vctrs_type("base_dispatch_class_str", vec_typeof(x));
  }
}

static SEXP s4_get_method(const char* cls, SEXP table) {
  SEXP sym = Rf_install(cls);

  // `r_env_get()` errors on missing bindings,
  // so we have to check with `r_env_has()`
  if (r_env_has(table, sym)) {
    SEXP method = r_env_get(table, sym);
    if (r_is_function(method)) {
      return method;
    }
  }

  return R_NilValue;
}

// For S4 objects, the `table` is specific to the generic
SEXP s4_find_method(SEXP x, SEXP table) {
  if (!IS_S4_OBJECT(x)) {
    return R_NilValue;
  }

  SEXP cls = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  SEXP out = s4_class_find_method(cls, table);

  UNPROTECT(1);
  return out;
}
SEXP s4_class_find_method(SEXP cls, SEXP table) {
  // Avoid corrupt objects where `x` is an OBJECT(), but the class is NULL
  if (cls == R_NilValue) {
    return R_NilValue;
  }

  SEXP const* p_class = STRING_PTR_RO(cls);
  int n_class = Rf_length(cls);

  for (int i = 0; i < n_class; ++i) {
    SEXP method = s4_get_method(CHAR(p_class[i]), table);
    if (method != R_NilValue) {
      return method;
    }
  }

  return R_NilValue;
}

// [[ include("utils.h") ]]
bool vec_implements_ptype2(SEXP x) {
  switch (vec_typeof(x)) {
  case VCTRS_TYPE_scalar:
    return false;
  case VCTRS_TYPE_s3: {
    SEXP method_sym = R_NilValue;
    SEXP method = s3_find_method_xy("vec_ptype2", x, x, vctrs_method_table, &method_sym);

    if (method != R_NilValue) {
      return true;
    }

    method = s3_find_method2("vec_ptype2", x, vctrs_method_table, &method_sym);
    return method != R_NilValue;
  }
  default:
    return true;
  }
}

// [[ register() ]]
SEXP vctrs_implements_ptype2(SEXP x) {
  return r_lgl(vec_implements_ptype2(x));
}

// [[ include("utils.h") ]]
SEXP list_first_non_null(SEXP xs, R_len_t* non_null_i) {
  SEXP x = R_NilValue;
  R_len_t n = Rf_length(xs);

  R_len_t i = 0;
  for (; i < n; ++i) {
    x = VECTOR_ELT(xs, i);
    if (x != R_NilValue) {
      break;
    }
  }

  if (non_null_i) {
    *non_null_i = i;
  }
  return x;
}

// [[ include("utils.h") ]]
SEXP node_compact_d(SEXP node) {
  SEXP handle = PROTECT(Rf_cons(R_NilValue, node));

  SEXP prev = handle;
  while (node != R_NilValue) {
    if (CAR(node) == R_NilValue) {
      SETCDR(prev, CDR(node));
    } else {
      prev = node;
    }
    node = CDR(node);
  }

  UNPROTECT(1);
  return CDR(handle);
}


// [[ include("utils.h") ]]
SEXP new_empty_factor(SEXP levels) {
  if (TYPEOF(levels) != STRSXP) {
    r_stop_internal("`level` must be a character vector.");
  }

  SEXP out = PROTECT(Rf_allocVector(INTSXP, 0));

  Rf_setAttrib(out, R_LevelsSymbol, levels);
  Rf_setAttrib(out, R_ClassSymbol, classes_factor);

  UNPROTECT(1);
  return out;
}

// [[ include("utils.h") ]]
SEXP new_empty_ordered(SEXP levels) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, 0));

  Rf_setAttrib(out, R_LevelsSymbol, levels);
  Rf_setAttrib(out, R_ClassSymbol, classes_ordered);

  UNPROTECT(1);
  return out;
}

// [[ include("utils.h") ]]
bool list_has_inner_vec_names(SEXP x, R_len_t size) {
  for (R_len_t i = 0; i < size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    if (vec_names(elt) != R_NilValue) {
      return true;
    }
  }

  return false;
}

/**
 * Pluck elements `i` from a list of lists.
 * @return A list of the same length as `xs`.
 */
// [[ include("utils.h") ]]
r_obj* list_pluck(r_obj* xs, r_ssize i) {
  r_ssize n = r_length(xs);
  r_obj* const * v_xs = r_list_cbegin(xs);

  r_obj* out = KEEP(r_new_list(n));

  for (r_ssize j = 0; j < n; ++j) {
    r_obj* x = v_xs[j];
    if (x != r_null) {
      r_list_poke(out, j, r_list_get(x, i));
    }
  }

  FREE(1);
  return out;
}


// Initialised at load time
SEXP compact_seq_attrib = NULL;

// p[0] = Start value
// p[1] = Sequence size. Always >= 0.
// p[2] = Step size to increment/decrement `start` with
void init_compact_seq(int* p, R_len_t start, R_len_t size, bool increasing) {
  int step = increasing ? 1 : -1;

  p[0] = start;
  p[1] = size;
  p[2] = step;
}

// Exported for testing with `list_combine()`
r_obj* ffi_compact_seq(r_obj* ffi_start, r_obj* ffi_size, r_obj* ffi_increasing) {
  return compact_seq(
    r_arg_as_ssize(ffi_start, "start"),
    r_arg_as_ssize(ffi_size, "size"),
    r_arg_as_bool(ffi_increasing, "increasing")
  );
}

// Returns a compact sequence that `vec_slice()` understands
// The sequence is generally generated as `[start, start +/- size)`
// If `size == 0` a 0-length sequence is generated
// `start` is 0-based
SEXP compact_seq(R_len_t start, R_len_t size, bool increasing) {
  if (start < 0) {
    r_stop_internal("`start` must not be negative.");
  }

  if (size < 0) {
    r_stop_internal("`size` must not be negative.");
  }

  if (!increasing && size > start + 1) {
    r_stop_internal("`size` must not be larger than `start` for decreasing sequences.");
  }

  SEXP info = PROTECT(Rf_allocVector(INTSXP, 3));

  int* p = INTEGER(info);
  init_compact_seq(p, start, size, increasing);

  SET_ATTRIB(info, compact_seq_attrib);

  UNPROTECT(1);
  return info;
}

bool is_compact_seq(SEXP x) {
  return ATTRIB(x) == compact_seq_attrib;
}

// Materialize a 1-based sequence
SEXP compact_seq_materialize(SEXP x) {
  int* p = INTEGER(x);
  R_len_t start = p[0] + 1;
  R_len_t size = p[1];
  R_len_t step = p[2];

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* out_data = INTEGER(out);

  for (R_len_t i = 0; i < size; ++i, ++out_data, start += step) {
    *out_data = start;
  }

  UNPROTECT(1);
  return out;
}

// Initialised at load time
SEXP compact_rep_attrib = NULL;

void init_compact_rep(int* p, R_len_t i, R_len_t n) {
  p[0] = i;
  p[1] = n;
}

// Returns a compact repetition that `vec_slice()` understands
// `i` should be an R-based index
SEXP compact_rep(R_len_t i, R_len_t n) {
  if (n < 0) {
    r_stop_internal("Negative `n` in `compact_rep()`.");
  }

  SEXP rep = PROTECT(Rf_allocVector(INTSXP, 2));

  int* p = INTEGER(rep);
  init_compact_rep(p, i, n);

  SET_ATTRIB(rep, compact_rep_attrib);

  UNPROTECT(1);
  return rep;
}

bool is_compact_rep(SEXP x) {
  return ATTRIB(x) == compact_rep_attrib;
}

SEXP compact_rep_materialize(SEXP x) {
  int i = r_int_get(x, 0);
  int n = r_int_get(x, 1);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  r_int_fill(out, i, n);

  UNPROTECT(1);
  return out;
}

// Initialised at load time
SEXP compact_condition_attrib = NULL;

/**
 * Compact condition index
 *
 * A condition index usable with `VCTRS_INDEX_STYLE_condition`
 * that is backed by a RAWSXP bool array rather than a LGLSXP.
 *
 * Extremely useful when you only have `true` and `false` values
 * and you construct the index at the C level (like `default`
 * locations in `list_combine()`).
 *
 * Using a bool array is 4x less memory than a LGLSXP, and is
 * faster due to being able to load more of the array into a
 * single cache line.
 */
r_obj* new_compact_condition(R_xlen_t size) {
  if (size < 0) {
    r_stop_internal("Negative `size` in `compact_condition()`.");
  }

  r_obj* out = KEEP(r_alloc_raw(size * sizeof(bool)));

  SET_ATTRIB(out, compact_condition_attrib);

  FREE(1);
  return out;
}

bool is_compact_condition(r_obj* x) {
  return ATTRIB(x) == compact_condition_attrib;
}

r_ssize compact_condition_size(r_obj* x) {
  // Should always be the same as the length, but you never know
  return r_length(x) / sizeof(bool);
}

// Materializes as its corresponding logical index.
// Maintains `index_style` of `VCTRS_INDEX_STYLE_condition`.
r_obj* compact_condition_materialize(r_obj* x) {
  const bool* v_x = compact_condition_cbegin(x);
  const r_ssize size = compact_condition_size(x);

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = v_x[i];
  }

  FREE(1);
  return out;
}

// Materializes as its corresponding `VCTRS_INDEX_STYLE_location` index
r_obj* compact_condition_materialize_location(r_obj* x) {
  const bool* v_x = compact_condition_cbegin(x);
  const r_ssize size = compact_condition_size(x);
  return p_bool_which(v_x, size);
}

bool* compact_condition_begin(r_obj* x) {
  return (bool*) r_raw_begin(x);
}
const bool* compact_condition_cbegin(r_obj* x) {
  return (const bool*) r_raw_cbegin(x);
}

r_ssize compact_condition_sum(r_obj* x) {
  const bool* v_x = compact_condition_cbegin(x);
  const r_ssize size = compact_condition_size(x);
  return p_bool_sum(v_x, size);
}

// Exported for testing with `vec_assign_compact_condition()`
r_obj* ffi_as_compact_condition(r_obj* x) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_stop_internal("`x` must be a logical condition vector.");
  }

  const r_ssize size = r_length(x);
  const int* v_x = r_lgl_cbegin(x);

  r_obj* out = KEEP(new_compact_condition(size));
  bool* v_out = compact_condition_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];

    if (elt == r_globals.na_int) {
      r_stop_internal("Can't use `NA` when creating a `compact_condition`.");
    }

    v_out[i] = elt;
  }

  FREE(1);
  return out;
}

// Materialize the subscript as its corresponding `index_style`
//
// - integer -> `VCTRS_INDEX_STYLE_location`
// - compact_rep -> `VCTRS_INDEX_STYLE_location`
// - compact_seq -> `VCTRS_INDEX_STYLE_location`
//
// - logical -> `VCTRS_INDEX_STYLE_condition`
// - compact_condition -> `VCTRS_INDEX_STYLE_condition`
SEXP vec_subscript_materialize(SEXP x) {
  if (is_compact_rep(x)) {
    return compact_rep_materialize(x);
  } else if (is_compact_seq(x)) {
    return compact_seq_materialize(x);
  } else if (is_compact_condition(x)) {
    return compact_condition_materialize(x);
  } else {
    return x;
  }
}

R_len_t vec_subscript_size(SEXP x) {
  if (is_compact_rep(x)) {
    return r_int_get(x, 1);
  } else if (is_compact_seq(x)) {
    return r_int_get(x, 1);
  } else if (is_compact_condition(x)) {
    return compact_condition_size(x);
  } else {
    return vec_size(x);
  }
}

r_ssize vec_condition_subscript_sum(r_obj* x, bool na_true) {
  if (is_compact_condition(x)) {
    return compact_condition_sum(x);
  } else {
    return r_lgl_sum(x, na_true);
  }
}

static SEXP syms_colnames = NULL;
static SEXP fns_colnames = NULL;

// [[ include("utils.h") ]]
SEXP colnames(SEXP x) {
  return vctrs_dispatch1(syms_colnames, fns_colnames,
                         syms_x, x);
}

r_obj* colnames2(r_obj* x) {
  r_obj* names = colnames(x);
  if (names == r_null) {
    return r_alloc_character(Rf_ncols(x));
  } else {
    return names;
  }
}

// [[ include("utils.h") ]]
bool is_integer64(SEXP x) {
  return TYPEOF(x) == REALSXP && Rf_inherits(x, "integer64");
}

// [[ include("utils.h") ]]
bool lgl_any_na(SEXP x) {
  R_xlen_t size = Rf_xlength(x);
  const int* p_x = LOGICAL_RO(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    if (p_x[i] == NA_LOGICAL) {
      return true;
    }
  }

  return false;
}

void* r_vec_deref_barrier(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP:
  case VECSXP:
    return (void*) x;
  default:
    return r_vec_begin(x);
  }
}

const void* r_vec_deref_barrier_const(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP:
  case VECSXP:
    return (const void*) x;
  default:
    return r_vec_cbegin(x);
  }
}

#define FILL(CTYPE, DEST, DEST_I, SRC, SRC_I, N)        \
  do {                                                  \
    CTYPE* p_dest = (CTYPE*) DEST;                      \
    p_dest += DEST_I;                                   \
    CTYPE* end = p_dest + N;                            \
    CTYPE value = ((const CTYPE*) SRC)[SRC_I];          \
                                                        \
    while (p_dest != end) {                             \
      *p_dest++ = value;                                \
    }                                                   \
  } while (false)

#define FILL_BARRIER(GET, SET, DEST, DEST_I, SRC, SRC_I, N)     \
  do {                                                          \
    SEXP out = (SEXP) DEST;                                     \
    SEXP value = GET((SEXP) SRC, SRC_I);                        \
                                                                \
    for (r_ssize i = 0; i < N; ++i) {                           \
      SET(out, DEST_I + i, value);                              \
    }                                                           \
  } while (false)

void r_vec_fill(SEXPTYPE type,
                void* dest,
                r_ssize dest_i,
                const void* src,
                r_ssize src_i,
                r_ssize n) {
  switch (type) {
  case INTSXP: FILL(int, dest, dest_i, src, src_i, n); return;
  case STRSXP: FILL_BARRIER(STRING_ELT, SET_STRING_ELT, dest, dest_i, src, src_i, n); return;
  default: stop_unimplemented_type("r_vec_fill", type);
  }
}

#undef FILL_BARRIER
#undef FILL


#define FILL() {                      \
  for (R_len_t i = 0; i < n; ++i) {   \
    p_x[i] = value;                   \
  }                                   \
}

void r_p_lgl_fill(int* p_x, int value, R_len_t n) {
  FILL();
}
void r_p_int_fill(int* p_x, int value, R_len_t n) {
  FILL();
}
void r_p_chr_fill(SEXP* p_x, SEXP value, R_len_t n) {
  FILL();
}

#undef FILL

void r_lgl_fill(SEXP x, int value, R_len_t n) {
  r_p_lgl_fill(LOGICAL(x), value, n);
}
void r_int_fill(SEXP x, int value, R_len_t n) {
  r_p_int_fill(INTEGER(x), value, n);
}


void r_int_fill_seq(SEXP x, int start, R_len_t n) {
  int* data = INTEGER(x);

  for (R_len_t i = 0; i < n; ++i, ++data, ++start) {
    *data = start;
  }
}

SEXP r_seq(R_len_t from, R_len_t to) {
  R_len_t n = to - from;
  if (n < 0) {
    r_stop_internal("Negative length.");
  }

  SEXP seq = PROTECT(Rf_allocVector(INTSXP, n));
  r_int_fill_seq(seq, from, n);

  UNPROTECT(1);
  return seq;
}


#define FIND(CTYPE, CONST_DEREF)                \
  R_len_t n = Rf_length(x);                     \
  const CTYPE* data = CONST_DEREF(x);           \
                                                \
  for (R_len_t i = 0; i < n; ++i) {             \
    if (data[i] == value) {                     \
      return i;                                 \
    }                                           \
  }                                             \
  return -1

R_len_t r_chr_find(SEXP x, SEXP value) {
  FIND(SEXP, STRING_PTR_RO);
}

#undef FIND


bool r_int_any_na(SEXP x) {
  int* data = INTEGER(x);
  R_len_t n = Rf_length(x);

  for (R_len_t i = 0; i < n; ++i, ++data) {
    if (*data == NA_INTEGER) {
      return true;
    }
  }

  return false;
}

// Treats missing values as `false`
bool r_lgl_any(r_obj* x) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_stop_internal("`x` must be a logical vector.");
  }

  const int* v_x = r_lgl_cbegin(x);
  r_ssize size = r_length(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i] == 1) {
      return true;
    }
  }

  return false;
}

// Like `!x` at the R level
r_obj* r_lgl_invert(r_obj* x) {
  const r_ssize size = r_length(x);
  const int* v_x = r_lgl_cbegin(x);

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];
    v_out[i] = (elt == r_globals.na_lgl) ? r_globals.na_lgl : !elt;
  }

  FREE(1);
  return out;
}

int r_chr_max_len(SEXP x) {
  R_len_t n = Rf_length(x);
  SEXP const* p = STRING_PTR_RO(x);

  int max = 0;
  for (R_len_t i = 0; i < n; ++i, ++p) {
    int len = strlen(CHAR(*p));
    max = len > max ? len : max;
  }

  return max;
}

/**
 * Create a character vector of sequential integers
 *
 * @param n The sequence is from 1 to `n`.
 * @param buf,len A memory buffer of size `len`.
 * @param prefix A null-terminated string that is prefixed to the
 *   sequence.
 */
SEXP r_chr_iota(R_len_t n, char* buf, int len, const char* prefix) {
  int prefix_len = strlen(prefix);
  if (len - 1 < prefix_len) {
    r_stop_internal("Prefix is larger than iota buffer.");
  }

  memcpy(buf, prefix, prefix_len);
  len -= prefix_len;
  char* beg = buf + prefix_len;

  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    int written = snprintf(beg, len, "%d", i + 1);

    if (written >= len) {
      UNPROTECT(1);
      return R_NilValue;
    }

    SET_STRING_ELT(out, i, Rf_mkChar(buf));
  }

  UNPROTECT(1);
  return out;
}


static SEXP new_env_call = NULL;
static SEXP new_env__parent_node = NULL;
static SEXP new_env__size_node = NULL;

// [[ include("utils.h") ]]
SEXP r_protect(SEXP x) {
  return Rf_lang2(fns_quote, x);
}

// [[ include("utils.h") ]]
int r_bool_as_int(SEXP x) {
  if (!r_is_bool(x)) {
    Rf_errorcall(R_NilValue, "Input must be a single `TRUE` or `FALSE`.");
  }
  return LOGICAL(x)[0];
}

bool r_is_number(SEXP x) {
  return TYPEOF(x) == INTSXP &&
    Rf_length(x) == 1 &&
    INTEGER(x)[0] != NA_INTEGER;
}
bool r_is_positive_number(SEXP x) {
  return r_is_number(x) && INTEGER(x)[0] > 0;
}


/**
 * Create a call or pairlist
 *
 * @param tags Optional. If not `NULL`, a null-terminated array of symbols.
 * @param cars Mandatory. A null-terminated array of CAR values.
 * @param fn The first CAR value of the language list.
 *
 * [[ include("utils.h") ]]
 */
SEXP _r_pairlist(SEXP* tags, SEXP* cars) {
  if (!cars) {
    r_stop_internal("NULL `cars`.");
  }

  SEXP list = PROTECT(Rf_cons(R_NilValue, R_NilValue));
  SEXP node = list;

  while (*cars) {
    SEXP next_node = Rf_cons(*cars, R_NilValue);
    SETCDR(node, next_node);
    node = next_node;

    if (tags) {
      SET_TAG(next_node, *tags);
      ++tags;
    }

    ++cars;
  }

  UNPROTECT(1);
  return CDR(list);
}
SEXP r_call_n(SEXP fn, SEXP* tags, SEXP* cars) {
  return Rf_lcons(fn, _r_pairlist(tags, cars));
}

bool r_has_name_at(SEXP names, R_len_t i) {
  if (TYPEOF(names) != STRSXP) {
    return false;
  }

  R_len_t n = Rf_length(names);
  if (n <= i) {
    r_stop_internal("Names shorter than expected: (%d/%d).", i + 1, n);
  }

  SEXP elt = STRING_ELT(names, i);
  return elt != NA_STRING && elt != strings_empty;
}

bool r_is_minimal_names(SEXP x) {
  if (TYPEOF(x) != STRSXP) {
    return false;
  }

  R_len_t n = Rf_length(x);
  const SEXP* p = STRING_PTR_RO(x);

  for (R_len_t i = 0; i < n; ++i, ++p) {
    SEXP elt = *p;
    if (elt == NA_STRING || elt == strings_empty) {
      return false;
    }
  }

  return true;
}

bool r_is_empty_names(SEXP x) {
  if (TYPEOF(x) != STRSXP) {
    if (x == R_NilValue) {
      return true;
    } else {
      return false;
    }
  }

  R_len_t n = Rf_length(x);
  const SEXP* p = STRING_PTR_RO(x);

  for (R_len_t i = 0; i < n; ++i, ++p) {
    SEXP elt = *p;
    if (elt != NA_STRING && elt != strings_empty) {
      return false;
    }
  }

  return true;
}

SEXP r_clone_referenced(SEXP x) {
  if (MAYBE_REFERENCED(x)) {
    return Rf_shallow_duplicate(x);
  } else {
    return x;
  }
}

bool r_is_names(SEXP names) {
  if (names == R_NilValue) {
    return false;
  }

  R_len_t n = Rf_length(names);
  const SEXP* p = STRING_PTR_RO(names);

  for (R_len_t i = 0; i < n; ++i, ++p) {
    SEXP nm = *p;
    if (nm == strings_empty || nm == NA_STRING) {
      return false;
    }
  }

  return true;
}

bool r_chr_has_string(SEXP x, SEXP str) {
  R_len_t n = Rf_length(x);
  const SEXP* xp = STRING_PTR_RO(x);

  for (R_len_t i = 0; i < n; ++i, ++xp) {
    if (*xp == str) {
      return true;
    }
  }

  return false;
}

SEXP r_as_data_frame(SEXP x) {
  if (is_bare_data_frame(x)) {
    return x;
  } else {
    return vctrs_dispatch1(syms_as_data_frame2, fns_as_data_frame2, syms_x, x);
  }
}

static SEXP syms_try_catch_hnd = NULL;
static inline SEXP try_catch_hnd(SEXP ptr) {
  SEXP call = PROTECT(Rf_lang2(syms_try_catch_hnd, ptr));
  SEXP out = Rf_eval(call, vctrs_ns_env);
  UNPROTECT(1);
  return out;
}

struct r_try_catch_data {
  void (*fn)(void*);
  void* fn_data;

  SEXP cnd_sym;

  void (*hnd)(void*);
  void* hnd_data;

  ERR err;
};

// [[ register() ]]
SEXP vctrs_try_catch_callback(SEXP ptr, SEXP cnd) {
  struct r_try_catch_data* data = (struct r_try_catch_data*) R_ExternalPtrAddr(ptr);

  if (cnd == R_NilValue) {
    if (data->fn) {
      data->fn(data->fn_data);
    }
  } else {
    data->err = cnd;
    if (data->hnd) {
      data->hnd(data->hnd_data);
    }
  }

  return R_NilValue;
}

static SEXP syms_try_catch_impl = NULL;

// [[ include("utils.h") ]]
ERR r_try_catch(void (*fn)(void*),
                void* fn_data,
                SEXP cnd_sym,
                void (*hnd)(void*),
                void* hnd_data) {

  struct r_try_catch_data data = {
    .fn = fn,
    .fn_data = fn_data,
    .cnd_sym = cnd_sym,
    .hnd = hnd,
    .hnd_data = hnd_data,
    .err = NULL
  };
  SEXP xptr = PROTECT(R_MakeExternalPtr(&data, R_NilValue, R_NilValue));
  SEXP hnd_fn = PROTECT(try_catch_hnd(xptr));

  SEXP syms[3] = {
    syms_data,
    cnd_sym,
    NULL
  };
  SEXP args[3] = {
    xptr,
    hnd_fn,
    NULL
  };

  SEXP call = PROTECT(r_call_n(syms_try_catch_impl, syms, args));
  Rf_eval(call, vctrs_ns_env);

  UNPROTECT(3);
  return data.err;
}

SEXP (*rlang_sym_as_character)(SEXP x);


// [[ include("utils.h") ]]
SEXP chr_c(SEXP x, SEXP y) {
  r_ssize x_n = r_length(x);
  r_ssize y_n = r_length(y);

  if (x_n == 0) {
    return y;
  }
  if (y_n == 0) {
    return x;
  }

  r_ssize out_n = r_ssize_add(x_n, y_n);
  SEXP out = PROTECT(r_alloc_vector(STRSXP, out_n));

  const SEXP* p_x = STRING_PTR_RO(x);
  const SEXP* p_y = STRING_PTR_RO(y);

  for (r_ssize i = 0; i < x_n; ++i) {
    SET_STRING_ELT(out, i, p_x[i]);
  }
  for (r_ssize i = 0, j = x_n; i < y_n; ++i, ++j) {
    SET_STRING_ELT(out, j, p_y[i]);
  }

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_fast_c(SEXP x, SEXP y) {
  SEXPTYPE x_type = TYPEOF(x);

  if (x_type != TYPEOF(y)) {
    Rf_error("`x` and `y` must have the same types.");
  }

  switch (x_type) {
  case STRSXP: return chr_c(x, y);
  default: stop_unimplemented_type("vctrs_fast_c", x_type);
  }
}


bool vctrs_debug_verbose = false;

SEXP vctrs_ns_env = NULL;
SEXP vctrs_shared_empty_str = NULL;

SEXP vctrs_shared_empty_date = NULL;

Rcomplex vctrs_shared_na_cpl;

SEXP vctrs_shared_missing_lgl = NULL;
SEXP vctrs_shared_missing_int = NULL;
SEXP vctrs_shared_missing_dbl = NULL;
SEXP vctrs_shared_missing_cpl = NULL;
SEXP vctrs_shared_missing_raw = NULL;
SEXP vctrs_shared_missing_chr = NULL;
SEXP vctrs_shared_missing_list = NULL;

SEXP vctrs_shared_zero_int = NULL;

SEXP strings2 = NULL;
SEXP strings_empty = NULL;
SEXP strings_dots = NULL;
SEXP strings_none = NULL;
SEXP strings_minimal = NULL;
SEXP strings_unique = NULL;
SEXP strings_universal = NULL;
SEXP strings_check_unique = NULL;
SEXP strings_unique_quiet = NULL;
SEXP strings_universal_quiet = NULL;
SEXP strings_key = NULL;
SEXP strings_loc = NULL;
SEXP strings_val = NULL;
SEXP strings_group = NULL;
SEXP strings_length = NULL;
SEXP strings_vctrs_vctr = NULL;
SEXP strings_times = NULL;
SEXP strings_needles = NULL;
SEXP strings_haystack = NULL;

SEXP chrs_subset = NULL;
SEXP chrs_extract = NULL;
SEXP chrs_assign = NULL;
SEXP chrs_rename = NULL;
SEXP chrs_remove = NULL;
SEXP chrs_negate = NULL;
SEXP chrs_null = NULL;
SEXP chrs_logical = NULL;
SEXP chrs_integer = NULL;
SEXP chrs_double = NULL;
SEXP chrs_complex = NULL;
SEXP chrs_character = NULL;
SEXP chrs_raw = NULL;
SEXP chrs_list = NULL;
SEXP chrs_expression = NULL;
SEXP chrs_numeric = NULL;
SEXP chrs_function = NULL;
SEXP chrs_empty = NULL;
SEXP chrs_cast = NULL;
SEXP chrs_error = NULL;
SEXP chrs_combine = NULL;
SEXP chrs_convert = NULL;
SEXP chrs_asc = NULL;
SEXP chrs_desc = NULL;
SEXP chrs_largest = NULL;
SEXP chrs_smallest = NULL;
SEXP chrs_which = NULL;

SEXP syms_i = NULL;
SEXP syms_j = NULL;
SEXP syms_n = NULL;
SEXP syms_x = NULL;
SEXP syms_y = NULL;
SEXP syms_x_size = NULL;
SEXP syms_y_size = NULL;
SEXP syms_to = NULL;
SEXP syms_dots = NULL;
SEXP syms_bracket = NULL;
SEXP syms_x_arg = NULL;
SEXP syms_y_arg = NULL;
SEXP syms_to_arg = NULL;
SEXP syms_times_arg = NULL;
SEXP syms_subscript_arg = NULL;
SEXP syms_needles_arg = NULL;
SEXP syms_haystack_arg = NULL;
SEXP syms_out = NULL;
SEXP syms_value = NULL;
SEXP syms_quiet = NULL;
SEXP syms_dot_name_spec = NULL;
SEXP syms_outer = NULL;
SEXP syms_inner = NULL;
SEXP syms_tilde = NULL;
SEXP syms_dot_environment = NULL;
SEXP syms_ptype = NULL;
SEXP syms_missing = NULL;
SEXP syms_size = NULL;
SEXP syms_subscript_action = NULL;
SEXP syms_subscript_type = NULL;
SEXP syms_repair = NULL;
SEXP syms_tzone = NULL;
SEXP syms_data = NULL;
SEXP syms_vctrs_error_incompatible_type = NULL;
SEXP syms_vctrs_error_cast_lossy = NULL;
SEXP syms_cnd_signal = NULL;
SEXP syms_logical = NULL;
SEXP syms_numeric = NULL;
SEXP syms_character = NULL;
SEXP syms_body = NULL;
SEXP syms_parent = NULL;
SEXP syms_s3_methods_table = NULL;
SEXP syms_from_dispatch = NULL;
SEXP syms_df_fallback = NULL;
SEXP syms_s3_fallback = NULL;
SEXP syms_stop_incompatible_type = NULL;
SEXP syms_stop_incompatible_size = NULL;
SEXP syms_stop_assert_size = NULL;
SEXP syms_stop_matches_overflow = NULL;
SEXP syms_stop_matches_nothing = NULL;
SEXP syms_stop_matches_remaining = NULL;
SEXP syms_stop_matches_incomplete = NULL;
SEXP syms_stop_matches_multiple = NULL;
SEXP syms_warn_matches_multiple = NULL;
SEXP syms_stop_matches_relationship_one_to_one = NULL;
SEXP syms_stop_matches_relationship_one_to_many = NULL;
SEXP syms_stop_matches_relationship_many_to_one = NULL;
SEXP syms_warn_matches_relationship_many_to_many = NULL;
SEXP syms_stop_combine_unmatched = NULL;
SEXP syms_action = NULL;
SEXP syms_vctrs_common_class_fallback = NULL;
SEXP syms_fallback_class = NULL;
SEXP syms_abort = NULL;
SEXP syms_message = NULL;
SEXP syms_chr_proxy_collate = NULL;
SEXP syms_actual = NULL;
SEXP syms_required = NULL;
SEXP syms_call = NULL;
SEXP syms_dot_call = NULL;
SEXP syms_which = NULL;
SEXP syms_slice_value = NULL;
SEXP syms_index_style = NULL;
SEXP syms_loc = NULL;

SEXP fns_bracket = NULL;
SEXP fns_quote = NULL;
SEXP fns_names = NULL;

SEXP result_attrib = NULL;


SEXP r_new_shared_vector(SEXPTYPE type, R_len_t n) {
  SEXP out = Rf_allocVector(type, n);
  R_PreserveObject(out);
  MARK_NOT_MUTABLE(out);
  return out;
}
SEXP r_new_shared_character(const char* name) {
  SEXP out = Rf_mkString(name);
  R_PreserveObject(out);
  MARK_NOT_MUTABLE(out);
  return out;
}

void c_print_backtrace(void) {
#if defined(RLIB_DEBUG)
#include <execinfo.h>
#include <stdlib.h>
  void *buffer[500];
  int nptrs = backtrace(buffer, 100);

  char **strings = backtrace_symbols(buffer, nptrs);
  for (int j = 0; j < nptrs; ++j) {
    Rprintf("%s\n", strings[j]);
  }

  free(strings);
#else
  Rprintf("vctrs must be compliled with -DRLIB_DEBUG.");
#endif
}


void vctrs_init_utils(SEXP ns) {
  vctrs_ns_env = ns;

  vctrs_debug_verbose = r_is_true(Rf_GetOption1(Rf_install("vctrs:::debug")));

  vctrs_method_table = r_env_get(ns, Rf_install(".__S3MethodsTable__."));
  base_method_table = r_env_get(R_BaseNamespace, Rf_install(".__S3MethodsTable__."));

  s4_c_method_table = r_parse_eval("environment(methods::getGeneric('c'))$.MTable", R_GlobalEnv);
  R_PreserveObject(s4_c_method_table);

  vctrs_shared_empty_str = Rf_mkString("");
  R_PreserveObject(vctrs_shared_empty_str);


  // Holds the CHARSXP objects because unlike symbols they can be
  // garbage collected
  strings2 = r_new_shared_vector(STRSXP, 25);

  strings_dots = Rf_mkChar("...");
  SET_STRING_ELT(strings2, 0, strings_dots);

  strings_empty = Rf_mkChar("");
  SET_STRING_ELT(strings2, 1, strings_empty);

  strings_date = Rf_mkChar("Date");
  SET_STRING_ELT(strings2, 2, strings_date);

  strings_posixct = Rf_mkChar("POSIXct");
  SET_STRING_ELT(strings2, 3, strings_posixct);

  strings_posixlt = Rf_mkChar("POSIXlt");
  SET_STRING_ELT(strings2, 4, strings_posixlt);

  strings_posixt = Rf_mkChar("POSIXt");
  SET_STRING_ELT(strings2, 5, strings_posixt);

  strings_none = Rf_mkChar("none");
  SET_STRING_ELT(strings2, 6, strings_none);

  strings_minimal = Rf_mkChar("minimal");
  SET_STRING_ELT(strings2, 7, strings_minimal);

  strings_unique = Rf_mkChar("unique");
  SET_STRING_ELT(strings2, 8, strings_unique);

  strings_universal = Rf_mkChar("universal");
  SET_STRING_ELT(strings2, 9, strings_universal);

  strings_check_unique = Rf_mkChar("check_unique");
  SET_STRING_ELT(strings2, 10, strings_check_unique);

  strings_unique_quiet = Rf_mkChar("unique_quiet");
  SET_STRING_ELT(strings2, 23, strings_unique_quiet);

  strings_universal_quiet = Rf_mkChar("universal_quiet");
  SET_STRING_ELT(strings2, 24, strings_universal_quiet);

  strings_key = Rf_mkChar("key");
  SET_STRING_ELT(strings2, 11, strings_key);

  strings_loc = Rf_mkChar("loc");
  SET_STRING_ELT(strings2, 12, strings_loc);

  strings_val = Rf_mkChar("val");
  SET_STRING_ELT(strings2, 13, strings_val);

  strings_group = Rf_mkChar("group");
  SET_STRING_ELT(strings2, 14, strings_group);

  strings_length = Rf_mkChar("length");
  SET_STRING_ELT(strings2, 15, strings_length);

  strings_factor = Rf_mkChar("factor");
  SET_STRING_ELT(strings2, 16, strings_factor);

  strings_ordered = Rf_mkChar("ordered");
  SET_STRING_ELT(strings2, 17, strings_ordered);

  strings_list = Rf_mkChar("list");
  SET_STRING_ELT(strings2, 18, strings_list);

  strings_vctrs_vctr = Rf_mkChar("vctrs_vctr");
  SET_STRING_ELT(strings2, 19, strings_vctrs_vctr);

  strings_times = Rf_mkChar("times");
  SET_STRING_ELT(strings2, 20, strings_times);

  strings_needles = Rf_mkChar("needles");
  SET_STRING_ELT(strings2, 21, strings_needles);

  strings_haystack = Rf_mkChar("haystack");
  SET_STRING_ELT(strings2, 22, strings_haystack);


  classes_data_frame = r_new_shared_vector(STRSXP, 1);
  strings_data_frame = Rf_mkChar("data.frame");
  SET_STRING_ELT(classes_data_frame, 0, strings_data_frame);

  classes_factor = r_new_shared_vector(STRSXP, 1);
  SET_STRING_ELT(classes_factor, 0, strings_factor);

  classes_ordered = r_new_shared_vector(STRSXP, 2);
  SET_STRING_ELT(classes_ordered, 0, strings_ordered);
  SET_STRING_ELT(classes_ordered, 1, strings_factor);

  classes_date = r_new_shared_vector(STRSXP, 1);
  SET_STRING_ELT(classes_date, 0, strings_date);

  classes_posixct = r_new_shared_vector(STRSXP, 2);
  SET_STRING_ELT(classes_posixct, 0, strings_posixct);
  SET_STRING_ELT(classes_posixct, 1, strings_posixt);

  chrs_subset = r_new_shared_character("subset");
  chrs_extract = r_new_shared_character("extract");
  chrs_assign = r_new_shared_character("assign");
  chrs_rename = r_new_shared_character("rename");
  chrs_remove = r_new_shared_character("remove");
  chrs_negate = r_new_shared_character("negate");
  chrs_null = r_new_shared_character("NULL");
  chrs_logical = r_new_shared_character("logical");
  chrs_integer = r_new_shared_character("integer");
  chrs_double = r_new_shared_character("double");
  chrs_complex = r_new_shared_character("complex");
  chrs_character = r_new_shared_character("character");
  chrs_raw = r_new_shared_character("raw");
  chrs_list = r_new_shared_character("list");
  chrs_expression = r_new_shared_character("expression");
  chrs_numeric = r_new_shared_character("numeric");
  chrs_function = r_new_shared_character("function");
  chrs_empty = r_new_shared_character("");
  chrs_cast = r_new_shared_character("cast");
  chrs_error = r_new_shared_character("error");
  chrs_combine = r_new_shared_character("combine");
  chrs_convert = r_new_shared_character("convert");
  chrs_asc = r_new_shared_character("asc");
  chrs_desc = r_new_shared_character("desc");
  chrs_largest = r_new_shared_character("largest");
  chrs_smallest = r_new_shared_character("smallest");
  chrs_which = r_new_shared_character("which");

  classes_tibble = r_new_shared_vector(STRSXP, 3);

  strings_tbl_df = Rf_mkChar("tbl_df");
  SET_STRING_ELT(classes_tibble, 0, strings_tbl_df);

  strings_tbl = Rf_mkChar("tbl");
  SET_STRING_ELT(classes_tibble, 1, strings_tbl);
  SET_STRING_ELT(classes_tibble, 2, strings_data_frame);


  classes_vctrs_group_rle = r_new_shared_vector(STRSXP, 3);
  SET_STRING_ELT(classes_vctrs_group_rle, 0, Rf_mkChar("vctrs_group_rle"));
  SET_STRING_ELT(classes_vctrs_group_rle, 1, Rf_mkChar("vctrs_rcrd"));
  SET_STRING_ELT(classes_vctrs_group_rle, 2, Rf_mkChar("vctrs_vctr"));


  vctrs_shared_empty_date = r_new_shared_vector(REALSXP, 0);
  Rf_setAttrib(vctrs_shared_empty_date, R_ClassSymbol, classes_date);

  vctrs_shared_na_cpl.i = NA_REAL;
  vctrs_shared_na_cpl.r = NA_REAL;

  vctrs_shared_missing_lgl = r_new_shared_vector(LGLSXP, 1);
  LOGICAL(vctrs_shared_missing_lgl)[0] = NA_LOGICAL;

  vctrs_shared_missing_int = r_new_shared_vector(INTSXP, 1);
  INTEGER(vctrs_shared_missing_int)[0] = NA_INTEGER;

  vctrs_shared_missing_dbl = r_new_shared_vector(REALSXP, 1);
  REAL(vctrs_shared_missing_dbl)[0] = NA_REAL;

  vctrs_shared_missing_cpl = r_new_shared_vector(CPLXSXP, 1);
  COMPLEX(vctrs_shared_missing_cpl)[0] = vctrs_shared_na_cpl;

  // No actual `NA` value for raw, but we always use `0`
  vctrs_shared_missing_raw = r_new_shared_vector(RAWSXP, 1);
  RAW(vctrs_shared_missing_raw)[0] = 0;

  vctrs_shared_missing_chr = r_new_shared_vector(STRSXP, 1);
  SET_STRING_ELT(vctrs_shared_missing_chr, 0, NA_STRING);

  vctrs_shared_missing_list = r_new_shared_vector(VECSXP, 1);
  SET_VECTOR_ELT(vctrs_shared_missing_list, 0, R_NilValue);

  vctrs_shared_zero_int = r_new_shared_vector(INTSXP, 1);
  INTEGER(vctrs_shared_zero_int)[0] = 0;

  syms_i = Rf_install("i");
  syms_j = Rf_install("j");
  syms_n = Rf_install("n");
  syms_x = Rf_install("x");
  syms_y = Rf_install("y");
  syms_x_size = Rf_install("x_size");
  syms_y_size = Rf_install("y_size");
  syms_to = Rf_install("to");
  syms_dots = Rf_install("...");
  syms_bracket = Rf_install("[");
  syms_x_arg = Rf_install("x_arg");
  syms_y_arg = Rf_install("y_arg");
  syms_to_arg = Rf_install("to_arg");
  syms_times_arg = Rf_install("times_arg");
  syms_subscript_arg = Rf_install("subscript_arg");
  syms_needles_arg = Rf_install("needles_arg");
  syms_haystack_arg = Rf_install("haystack_arg");
  syms_out = Rf_install("out");
  syms_value = Rf_install("value");
  syms_quiet = Rf_install("quiet");
  syms_dot_name_spec = Rf_install(".name_spec");
  syms_outer = Rf_install("outer");
  syms_inner = Rf_install("inner");
  syms_tilde = Rf_install("~");
  syms_dot_environment = Rf_install(".Environment");
  syms_ptype = Rf_install("ptype");
  syms_missing = R_MissingArg;
  syms_size = Rf_install("size");
  syms_subscript_action = Rf_install("subscript_action");
  syms_subscript_type = Rf_install("subscript_type");
  syms_repair = Rf_install("repair");
  syms_tzone = Rf_install("tzone");
  syms_data = Rf_install("data");
  syms_try_catch_impl = Rf_install("try_catch_impl");
  syms_try_catch_hnd = Rf_install("try_catch_hnd");
  syms_vctrs_error_incompatible_type = Rf_install("vctrs_error_incompatible_type");
  syms_vctrs_error_cast_lossy = Rf_install("vctrs_error_cast_lossy");
  syms_cnd_signal = Rf_install("cnd_signal");
  syms_logical = Rf_install("logical");
  syms_numeric = Rf_install("numeric");
  syms_character = Rf_install("character");
  syms_body = Rf_install("body");
  syms_parent = Rf_install("parent");
  syms_s3_methods_table = Rf_install(".__S3MethodsTable__.");
  syms_from_dispatch = Rf_install("vctrs:::from_dispatch");
  syms_s3_fallback = Rf_install("vctrs:::s3_fallback");
  syms_stop_incompatible_type = Rf_install("stop_incompatible_type");
  syms_stop_incompatible_size = Rf_install("stop_incompatible_size");
  syms_stop_assert_size = Rf_install("stop_assert_size");
  syms_stop_matches_overflow = Rf_install("stop_matches_overflow");
  syms_stop_matches_nothing = Rf_install("stop_matches_nothing");
  syms_stop_matches_remaining = Rf_install("stop_matches_remaining");
  syms_stop_matches_incomplete = Rf_install("stop_matches_incomplete");
  syms_stop_matches_multiple = Rf_install("stop_matches_multiple");
  syms_warn_matches_multiple = Rf_install("warn_matches_multiple");
  syms_stop_matches_relationship_one_to_one = Rf_install("stop_matches_relationship_one_to_one");
  syms_stop_matches_relationship_one_to_many = Rf_install("stop_matches_relationship_one_to_many");
  syms_stop_matches_relationship_many_to_one = Rf_install("stop_matches_relationship_many_to_one");
  syms_warn_matches_relationship_many_to_many = Rf_install("warn_matches_relationship_many_to_many");
  syms_stop_combine_unmatched = Rf_install("stop_combine_unmatched");
  syms_action = Rf_install("action");
  syms_vctrs_common_class_fallback = Rf_install(c_strs_vctrs_common_class_fallback);
  syms_fallback_class = Rf_install("fallback_class");
  syms_abort = Rf_install("abort");
  syms_message = Rf_install("message");
  syms_chr_proxy_collate = Rf_install("chr_proxy_collate");
  syms_actual = Rf_install("actual");
  syms_required = Rf_install("required");
  syms_call = Rf_install("call");
  syms_dot_call = Rf_install(".call");
  syms_which = Rf_install("which");
  syms_slice_value = Rf_install("slice_value");
  syms_index_style = Rf_install("index_style");
  syms_loc = Rf_install("loc");

  fns_bracket = Rf_findVar(syms_bracket, R_BaseEnv);
  fns_quote = Rf_findVar(Rf_install("quote"), R_BaseEnv);
  fns_names = Rf_findVar(Rf_install("names"), R_BaseEnv);

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);

  rlang_sym_as_character = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_sym_as_character");

  syms_as_data_frame2 = Rf_install("as.data.frame2");
  syms_colnames = Rf_install("colnames");

  fns_as_data_frame2 = r_env_get(ns, syms_as_data_frame2);
  fns_colnames = r_env_get(R_BaseEnv, syms_colnames);

  compact_seq_attrib = Rf_cons(R_NilValue, R_NilValue);
  R_PreserveObject(compact_seq_attrib);
  SET_TAG(compact_seq_attrib, Rf_install("vctrs_compact_seq"));

  compact_rep_attrib = Rf_cons(R_NilValue, R_NilValue);
  R_PreserveObject(compact_rep_attrib);
  SET_TAG(compact_rep_attrib, Rf_install("vctrs_compact_rep"));

  compact_condition_attrib = Rf_cons(R_NilValue, R_NilValue);
  R_PreserveObject(compact_condition_attrib);
  SET_TAG(compact_condition_attrib, Rf_install("vctrs_compact_condition"));

  {
    SEXP result_names = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(result_names, 0, Rf_mkChar("ok"));
    SET_STRING_ELT(result_names, 1, Rf_mkChar("err"));

    result_attrib = PROTECT(Rf_cons(result_names, R_NilValue));
    SET_TAG(result_attrib, R_NamesSymbol);

    SEXP result_class = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(result_class, 0, Rf_mkChar("rlang_result"));

    result_attrib = PROTECT(Rf_cons(result_class, result_attrib));
    SET_TAG(result_attrib, R_ClassSymbol);

    R_PreserveObject(result_attrib);
    MARK_NOT_MUTABLE(result_attrib);
    UNPROTECT(4);
  }

  // We assume the following in `union vctrs_dbl_indicator`
  VCTRS_ASSERT(sizeof(double) == sizeof(int64_t));
  VCTRS_ASSERT(sizeof(double) == 2 * sizeof(int));

  // We assume the following in `vec_order()`
  VCTRS_ASSERT(sizeof(int) == sizeof(int32_t));
  VCTRS_ASSERT(sizeof(double) == sizeof(int64_t));
}
