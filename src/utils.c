#include "vctrs.h"
#include "utils.h"

#include <R_ext/Rdynload.h>

// Initialised at load time
bool (*rlang_is_splice_box)(SEXP) = NULL;
SEXP (*rlang_unbox)(SEXP) = NULL;
SEXP (*rlang_env_dots_values)(SEXP) = NULL;
SEXP (*rlang_env_dots_list)(SEXP) = NULL;
SEXP vctrs_method_table = NULL;
SEXP base_method_table = NULL;
SEXP s4_c_method_table = NULL;

SEXP strings_tbl = NULL;
SEXP strings_tbl_df = NULL;
SEXP strings_data_frame = NULL;
SEXP strings_vctrs_rcrd = NULL;
SEXP strings_date = NULL;
SEXP strings_posixct = NULL;
SEXP strings_posixlt = NULL;
SEXP strings_posixt = NULL;
SEXP strings_factor = NULL;
SEXP strings_ordered = NULL;
SEXP strings_vctrs_vctr = NULL;
SEXP strings_vctrs_list_of = NULL;
SEXP strings_list = NULL;

SEXP classes_data_frame = NULL;
SEXP classes_factor = NULL;
SEXP classes_ordered = NULL;
SEXP classes_date = NULL;
SEXP classes_posixct = NULL;
SEXP classes_tibble = NULL;
SEXP classes_list_of = NULL;
SEXP classes_vctrs_group_rle = NULL;

static SEXP syms_as_data_frame2 = NULL;
static SEXP fns_as_data_frame2 = NULL;


static SEXP vctrs_eval_mask_n_impl(SEXP fn, SEXP* syms, SEXP* args, SEXP mask);

/**
 * Evaluate with masked arguments
 *
 * This takes two arrays of argument (`args`) and argument names
 * `syms`). The names should correspond to formal arguments of `fn`.
 * Elements of `args` are assigned to their corresponding name in
 * `syms` in a child of `env`. A call to `fn` is constructed with the
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
SEXP vctrs_eval_mask_n(SEXP fn, SEXP* syms, SEXP* args, SEXP env) {
  SEXP mask = PROTECT(r_new_environment(env, 4));
  SEXP out = vctrs_eval_mask_n_impl(fn, syms, args, mask);

  UNPROTECT(1);
  return out;
}
SEXP vctrs_eval_mask1(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP env) {
  SEXP syms[2] = { x_sym, NULL };
  SEXP args[2] = { x, NULL };
  return vctrs_eval_mask_n(fn, syms, args, env);
}
SEXP vctrs_eval_mask2(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP y_sym, SEXP y,
                      SEXP env) {
  SEXP syms[3] = { x_sym, y_sym, NULL };
  SEXP args[3] = { x, y, NULL };
  return vctrs_eval_mask_n(fn, syms, args, env);
}
SEXP vctrs_eval_mask3(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP y_sym, SEXP y,
                      SEXP z_sym, SEXP z,
                      SEXP env) {
  SEXP syms[4] = { x_sym, y_sym, z_sym, NULL };
  SEXP args[4] = { x, y, z, NULL };
  return vctrs_eval_mask_n(fn, syms, args, env);
}
SEXP vctrs_eval_mask4(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP env) {
  SEXP syms[5] = { x1_sym, x2_sym, x3_sym, x4_sym, NULL };
  SEXP args[5] = { x1, x2, x3, x4, NULL };
  return vctrs_eval_mask_n(fn, syms, args, env);
}
SEXP vctrs_eval_mask5(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP x5_sym, SEXP x5,
                      SEXP env) {
  SEXP syms[6] = { x1_sym, x2_sym, x3_sym, x4_sym, x5_sym, NULL };
  SEXP args[6] = { x1, x2, x3, x4, x5, NULL };
  return vctrs_eval_mask_n(fn, syms, args, env);
}

/**
 * Dispatch in the global environment
 *
 * Like `vctrs_eval_mask_n()`, the arguments `args` are are assigned
 * to the symbols `syms`. In addition, the function `fn` is assigned
 * to `fn_sym`. The mask is a direct child of the global environment
 * so that method dispatch finds globally defined methods.
 *
 * @param fn_sym A symbol to which `fn` is assigned.
 * @inheritParams vctrs_eval_mask_n
 */
SEXP vctrs_dispatch_n(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args) {
  // Mask `fn` with `fn_sym`. We dispatch in the global environment.
  SEXP mask = PROTECT(r_new_environment(R_GlobalEnv, 4));
  Rf_defineVar(fn_sym, fn, mask);

  SEXP out = vctrs_eval_mask_n_impl(fn_sym, syms, args, mask);

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

static SEXP vctrs_eval_mask_n_impl(SEXP fn, SEXP* syms, SEXP* args, SEXP mask) {
  SEXP call = PROTECT(r_call(fn, syms, syms));

  while (*syms) {
    Rf_defineVar(*syms, *args, mask);
    ++syms; ++args;
  }

  SEXP out = Rf_eval(call, mask);

  UNPROTECT(1);
  return out;
}

// An alternative to `attributes(x) <- attrib`, which makes
// two copies on R < 3.6.0
// [[ register() ]]
SEXP vctrs_set_attributes(SEXP x, SEXP attrib) {
  R_len_t n_attrib = Rf_length(attrib);
  int n_protect = 0;

  if (MAYBE_REFERENCED(x)) {
    x = PROTECT(Rf_shallow_duplicate(x));
    ++n_protect;
  }

  // Remove existing attributes, and unset the object bit
  SET_ATTRIB(x, R_NilValue);
  SET_OBJECT(x, 0);

  // Possible early exit after removing attributes
  if (n_attrib == 0) {
    UNPROTECT(n_protect);
    return x;
  }

  SEXP names = Rf_getAttrib(attrib, R_NamesSymbol);

  if (Rf_isNull(names)) {
    Rf_errorcall(R_NilValue, "Attributes must be named.");
  }

  // Check that each element of `names` is named.
  for (R_len_t i = 0; i < n_attrib; ++i) {
    SEXP name = STRING_ELT(names, i);

    if (name == NA_STRING || name == R_BlankString) {
      const char* msg = "All attributes must have names. Attribute %i does not.";
      Rf_errorcall(R_NilValue, msg, i + 1);
    }
  }

  // Always set `dim` first, if it exists. This way it is set before `dimnames`.
  int dim_pos = -1;
  for (R_len_t i = 0; i < n_attrib; ++i) {
    if (!strcmp(CHAR(STRING_ELT(names, i)), "dim")) {
      dim_pos = i;
      break;
    }
  }

  if (dim_pos != -1) {
    Rf_setAttrib(x, R_DimSymbol, VECTOR_ELT(attrib, dim_pos));
  }

  for (R_len_t i = 0; i < n_attrib; ++i) {
    if (i == dim_pos) {
      continue;
    }
    Rf_setAttrib(x, Rf_installChar(STRING_ELT(names, i)), VECTOR_ELT(attrib, i));
  }

  UNPROTECT(n_protect);
  return x;
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
  out = vec_bare_df_restore(out, df, vctrs_shared_zero_int);

  UNPROTECT(1);
  return out;
}

// [[ include("utils.h") ]]
SEXP df_map(SEXP df, SEXP (*fn)(SEXP)) {
  SEXP out = PROTECT(map(df, fn));
  out = vec_df_restore(out, df, vctrs_shared_zero_int);

  UNPROTECT(1);
  return out;
}

inline void never_reached(const char* fn) {
  Rf_error("Internal error in `%s()`: Reached the unreachable.", fn);
}


static char s3_buf[200];

static SEXP s3_method_sym(const char* generic, const char* class) {
  int gen_len = strlen(generic);
  int class_len = strlen(class);
  int dot_len = 1;
  if (gen_len + class_len + dot_len >= 200) {
    Rf_error("Internal error: Generic or class name is too long.");
  }

  char* buf = s3_buf;

  memcpy(buf, generic, gen_len); buf += gen_len;
  *buf = '.'; ++buf;
  memcpy(buf, class, class_len); buf += class_len;
  *buf = '\0';

  return Rf_install(s3_buf);
}

// First check in global env, then in method table
static SEXP s3_get_method(const char* generic, const char* class, SEXP table) {
  SEXP sym = s3_method_sym(generic, class);

  SEXP method = r_env_get(R_GlobalEnv, sym);
  if (r_is_function(method)) {
    return method;
  }

  method = r_env_get(table, sym);
  if (r_is_function(method)) {
    return method;
  }

  return R_NilValue;
}

SEXP s3_find_method(const char* generic, SEXP x, SEXP table) {
  if (!OBJECT(x)) {
    return R_NilValue;
  }

  SEXP class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));

  // Avoid corrupt objects where `x` is an OBJECT(), but the class is NULL
  if (class == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP* class_ptr = STRING_PTR(class);
  int n_class = Rf_length(class);

  for (int i = 0; i < n_class; ++i, ++class_ptr) {
    SEXP method = s3_get_method(generic, CHAR(*class_ptr), table);
    if (method != R_NilValue) {
      UNPROTECT(1);
      return method;
    }
  }

  UNPROTECT(1);
  return R_NilValue;
}

static SEXP s4_get_method(const char* class, SEXP table) {
  SEXP sym = Rf_install(class);

  SEXP method = r_env_get(table, sym);
  if (r_is_function(method)) {
    return method;
  }

  return R_NilValue;
}

// For S4 objects, the `table` is specific to the generic
SEXP s4_find_method(SEXP x, SEXP table) {
  if (!IS_S4_OBJECT(x)) {
    return R_NilValue;
  }

  SEXP class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));

  // Avoid corrupt objects where `x` is an OBJECT(), but the class is NULL
  if (class == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP* p_class = STRING_PTR(class);
  int n_class = Rf_length(class);

  for (int i = 0; i < n_class; ++i) {
    SEXP method = s4_get_method(CHAR(p_class[i]), table);
    if (method != R_NilValue) {
      UNPROTECT(1);
      return method;
    }
  }

  UNPROTECT(1);
  return R_NilValue;
}

// [[ include("utils.h") ]]
bool vec_implements_ptype2(SEXP x) {
  if (vec_typeof(x) == vctrs_type_s3) {
    SEXP met = s3_find_method("vec_ptype2", x, vctrs_method_table);
    return met != R_NilValue;
  } else {
    return true;
  }
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
bool list_is_homogeneously_classed(SEXP xs) {
  R_len_t n = Rf_length(xs);
  if (n == 0 || n == 1) {
    return true;
  }

  R_len_t i = -1;
  SEXP first = list_first_non_null(xs, &i);
  SEXP first_class = PROTECT(r_class(first));

  for (; i < n; ++i) {
    SEXP this = VECTOR_ELT(xs, i);
    if (this == R_NilValue) {
      continue;
    }
    SEXP this_class = PROTECT(r_class(this));

    if (!equal_object(first_class, this_class)) {
      UNPROTECT(2);
      return false;
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
  return true;
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
    Rf_errorcall(R_NilValue, "Internal error: `level` must be a character vector.");
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

// [[ include("vctrs.h") ]]
enum vctrs_dbl_class dbl_classify(double x) {
  if (!isnan(x)) {
    return vctrs_dbl_number;
  }

  union vctrs_dbl_indicator indicator;
  indicator.value = x;

  if (indicator.key[vctrs_indicator_pos] == 1954) {
    return vctrs_dbl_missing;
  } else {
    return vctrs_dbl_nan;
  }
}

// Initialised at load time
SEXP compact_seq_attrib = NULL;

// p[0] = Start value
// p[1] = Sequence size. Always >= 1.
// p[2] = Step size to increment/decrement `start` with
void init_compact_seq(int* p, R_len_t start, R_len_t size, bool increasing) {
  int step = increasing ? 1 : -1;

  p[0] = start;
  p[1] = size;
  p[2] = step;
}

// Returns a compact sequence that `vec_slice()` understands
// The sequence is generally generated as `[start, start +/- size)`
// If `size == 0` a 0-length sequence is generated
// `start` is 0-based
SEXP compact_seq(R_len_t start, R_len_t size, bool increasing) {
  if (start < 0) {
    Rf_error("Internal error: `start` must not be negative in `compact_seq()`.");
  }

  if (size < 0) {
    Rf_error("Internal error: `size` must not be negative in `compact_seq()`.");
  }

  if (!increasing && size > start + 1) {
    Rf_error("Internal error: If constructing a decreasing sequence, `size` must not be larger than `start` in `compact_seq()`.");
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
    Rf_error("Internal error: Negative `n` in `compact_rep()`.");
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

bool is_compact(SEXP x) {
  return is_compact_rep(x) || is_compact_seq(x);
}

SEXP compact_materialize(SEXP x) {
  if (is_compact_rep(x)) {
    return compact_rep_materialize(x);
  } else if (is_compact_seq(x)) {
    return compact_seq_materialize(x);
  } else {
    return x;
  }
}

R_len_t vec_subscript_size(SEXP x) {
  if (is_compact_rep(x)) {
    return r_int_get(x, 1);
  } else if (is_compact_seq(x)) {
    return r_int_get(x, 1);
  } else {
    return vec_size(x);
  }
}

static SEXP syms_colnames = NULL;
static SEXP fns_colnames = NULL;

// [[ include("utils.h") ]]
SEXP colnames(SEXP x) {
  return vctrs_dispatch1(syms_colnames, fns_colnames,
                         syms_x, x);
}

// [[ include("utils.h") ]]
bool is_integer64(SEXP x) {
  return TYPEOF(x) == REALSXP && Rf_inherits(x, "integer64");
}


void* r_vec_deref(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP: return LOGICAL(x);
  case INTSXP: return INTEGER(x);
  case REALSXP: return REAL(x);
  case CPLXSXP: return COMPLEX(x);
  case STRSXP: return STRING_PTR(x);
  case RAWSXP: return RAW(x);
  default: Rf_error("Unimplemented type in `r_vec_deref()`.");
  }
}

const void* r_vec_const_deref(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return INTEGER_RO(x);
  case STRSXP: return STRING_PTR_RO(x);
  default: Rf_error("Unimplemented type in `r_vec_deref()`.");
  }
}

void r_vec_ptr_inc(SEXPTYPE type, void** p, R_len_t i) {
  switch (type) {
  case STRSXP: *((SEXP**) p) += i; return;
  case INTSXP: *((int**) p) += i; return;
  default: Rf_error("Unimplemented type in `r_vec_ptr_inc()`.");
  }
}

#define FILL(CTYPE, PTR, VAL_PTR, VAL_I, N)             \
  do {                                                  \
    CTYPE* data = (CTYPE*) PTR;                         \
    CTYPE* end = data + N;                              \
    CTYPE value = ((const CTYPE*) VAL_PTR)[VAL_I];      \
                                                        \
    while (data != end) {                               \
      *data++ = value;                                  \
    }                                                   \
  } while (false)

void r_vec_fill(SEXPTYPE type, void* p, const void* value_p, R_len_t value_i, R_len_t n) {
  switch (type) {
  case STRSXP: FILL(SEXP, p, value_p, value_i, n); return;
  case INTSXP: FILL(int, p, value_p, value_i, n); return;
  default: Rf_error("Internal error: Unimplemented type in `r_fill()`");
  }
}

#undef FILL


R_len_t r_lgl_sum(SEXP x, bool na_true) {
  if (TYPEOF(x) != LGLSXP) {
    Rf_errorcall(R_NilValue, "Internal error: Excepted logical vector in `r_lgl_sum()`");
  }

  R_len_t n = Rf_length(x);

  R_len_t sum = 0;
  int* ptr = LOGICAL(x);

  for (R_len_t i = 0; i < n; ++i, ++ptr) {
    // This can't overflow since `sum` is necessarily smaller or equal
    // to the vector length expressed in `R_len_t`.
    if (na_true && *ptr) {
      sum += 1;
    } else if (*ptr == 1) {
      sum += 1;
    }
  }

  return sum;
}

SEXP r_lgl_which(SEXP x, bool na_propagate) {
  if (TYPEOF(x) != LGLSXP) {
    Rf_errorcall(R_NilValue, "Internal error: Expected logical vector in `r_lgl_which()`");
  }

  R_len_t n = Rf_length(x);
  int* data = LOGICAL(x);

  R_len_t which_n = r_lgl_sum(x, na_propagate);
  SEXP which = PROTECT(Rf_allocVector(INTSXP, which_n));
  int* which_data = INTEGER(which);

  for (R_len_t i = 0; i < n; ++i, ++data) {
    int elt = *data;

    if (elt) {
      if (na_propagate && elt == NA_LOGICAL) {
        *which_data++ = NA_INTEGER;
      } else if (elt != NA_LOGICAL) {
        *which_data++ = i + 1;
      }
    }
  }

  UNPROTECT(1);
  return which;
}


#define FILL(CTYPE, DEREF)                      \
  CTYPE* data = DEREF(x);                       \
                                                \
  for (R_len_t i = 0; i < n; ++i, ++data)       \
    *data = value

void r_lgl_fill(SEXP x, int value, R_len_t n) {
  FILL(int, LOGICAL);
}
void r_int_fill(SEXP x, int value, R_len_t n) {
  FILL(int, INTEGER);
}
void r_chr_fill(SEXP x, SEXP value, R_len_t n) {
  FILL(SEXP, STRING_PTR);
}

#undef FILL


void r_int_fill_seq(SEXP x, int start, R_len_t n) {
  int* data = INTEGER(x);

  for (R_len_t i = 0; i < n; ++i, ++data, ++start) {
    *data = start;
  }
}

SEXP r_seq(R_len_t from, R_len_t to) {
  R_len_t n = to - from;
  if (n < 0) {
    Rf_error("Internal error: Negative length in `r_seq()`");
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


int r_chr_max_len(SEXP x) {
  R_len_t n = Rf_length(x);
  SEXP* p = STRING_PTR(x);

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
    Rf_errorcall(R_NilValue, "Internal error: Prefix is larger than iota buffer.");
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


#include <R_ext/Parse.h>

static void abort_parse(SEXP code, const char* why) {
  if (Rf_GetOption1(Rf_install("rlang__verbose_errors")) != R_NilValue) {
   Rf_PrintValue(code);
  }
  Rf_error("Internal error: %s", why);
}

SEXP r_parse(const char* str) {
  SEXP str_ = PROTECT(Rf_mkString(str));

  ParseStatus status;
  SEXP out = PROTECT(R_ParseVector(str_, -1, &status, R_NilValue));
  if (status != PARSE_OK) {
    abort_parse(str_, "Parsing failed");
  }
  if (Rf_length(out) != 1) {
    abort_parse(str_, "Expected a single expression");
  }

  out = VECTOR_ELT(out, 0);

  UNPROTECT(2);
  return out;
}
SEXP r_parse_eval(const char* str, SEXP env) {
  SEXP out = Rf_eval(PROTECT(r_parse(str)), env);
  UNPROTECT(1);
  return out;
}

static SEXP new_env_call = NULL;
static SEXP new_env__parent_node = NULL;
static SEXP new_env__size_node = NULL;

SEXP r_new_environment(SEXP parent, R_len_t size) {
  parent = parent ? parent : R_EmptyEnv;
  SETCAR(new_env__parent_node, parent);

  size = size ? size : 29;
  SETCAR(new_env__size_node, Rf_ScalarInteger(size));

  SEXP env = Rf_eval(new_env_call, R_BaseEnv);

  // Free for gc
  SETCAR(new_env__parent_node, R_NilValue);

  return env;
}

static SEXP new_function_call = NULL;
static SEXP new_function__formals_node = NULL;
static SEXP new_function__body_node = NULL;

SEXP r_new_function(SEXP formals, SEXP body, SEXP env) {
  SETCAR(new_function__formals_node, formals);
  SETCAR(new_function__body_node, body);

  SEXP fn = Rf_eval(new_function_call, env);

  // Free for gc
  SETCAR(new_function__formals_node, R_NilValue);
  SETCAR(new_function__body_node, R_NilValue);

  return fn;
}

// [[ include("utils.h") ]]
SEXP r_protect(SEXP x) {
  return Rf_lang2(fns_quote, x);
}

// [[ include("utils.h") ]]
bool r_is_bool(SEXP x) {
  return
    TYPEOF(x) == LGLSXP &&
    Rf_length(x) == 1 &&
    LOGICAL(x)[0] != NA_LOGICAL;
}
bool r_is_true(SEXP x) {
  return r_is_bool(x) && LOGICAL(x)[0] == 1;
}

// [[ include("utils.h") ]]
int r_bool_as_int(SEXP x) {
  if (!r_is_bool(x)) {
    Rf_errorcall(R_NilValue, "Input must be a single `TRUE` or `FALSE`.");
  }
  return LOGICAL(x)[0];
}

bool r_is_string(SEXP x) {
  return TYPEOF(x) == STRSXP &&
    Rf_length(x) == 1 &&
    STRING_ELT(x, 0) != NA_STRING;
}
bool r_is_number(SEXP x) {
  return TYPEOF(x) == INTSXP &&
    Rf_length(x) == 1 &&
    INTEGER(x)[0] != NA_INTEGER;
}

SEXP r_peek_option(const char* option) {
  return Rf_GetOption1(Rf_install(option));
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
SEXP r_pairlist(SEXP* tags, SEXP* cars) {
  if (!cars) {
    Rf_error("Internal error: Null `cars` in `r_pairlist()`");
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
SEXP r_call(SEXP fn, SEXP* tags, SEXP* cars) {
  return Rf_lcons(fn, r_pairlist(tags, cars));
}

bool r_has_name_at(SEXP names, R_len_t i) {
  if (TYPEOF(names) != STRSXP) {
    return false;
  }

  R_len_t n = Rf_length(names);
  if (n <= i) {
    Rf_error("Internal error: Names shorter than expected: (%d/%d)", i + 1, n);
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

SEXP r_env_get(SEXP env, SEXP sym) {
  SEXP obj = PROTECT(Rf_findVarInFrame3(env, sym, FALSE));

  // Force lazy loaded bindings
  if (TYPEOF(obj) == PROMSXP) {
    obj = Rf_eval(obj, R_BaseEnv);
  }

  UNPROTECT(1);
  return obj;
}

bool r_is_function(SEXP x) {
  switch (TYPEOF(x)) {
  case CLOSXP:
  case BUILTINSXP:
  case SPECIALSXP:
    return true;
  default:
    return false;
  }
}

SEXP r_maybe_duplicate(SEXP x) {
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

SEXP rlang_formula_formals = NULL;

SEXP r_as_function(SEXP x, const char* arg) {
  switch (TYPEOF(x)) {
  case CLOSXP:
  case BUILTINSXP:
  case SPECIALSXP:
    return x;
  case LANGSXP:
    if (CAR(x) == syms_tilde && CDDR(x) == R_NilValue) {
      SEXP env = PROTECT(Rf_getAttrib(x, syms_dot_environment));
      if (env == R_NilValue) {
        Rf_errorcall(R_NilValue, "Can't transform formula to function because it doesn't have an environment.");
      }

      SEXP fn = r_new_function(rlang_formula_formals, CADR(x), env);

      UNPROTECT(1);
      return fn;
    }
    // else fallthrough;
  default:
    Rf_errorcall(R_NilValue, "Can't convert `%s` to a function", arg);
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

  SEXP call = PROTECT(r_call(syms_try_catch_impl, syms, args));
  Rf_eval(call, vctrs_ns_env);

  UNPROTECT(3);
  return data.err;
}

SEXP (*rlang_sym_as_character)(SEXP x);


SEXP vctrs_ns_env = NULL;
SEXP vctrs_shared_empty_str = NULL;

SEXP vctrs_shared_empty_lgl = NULL;
SEXP vctrs_shared_empty_int = NULL;
SEXP vctrs_shared_empty_dbl = NULL;
SEXP vctrs_shared_empty_cpl = NULL;
SEXP vctrs_shared_empty_chr = NULL;
SEXP vctrs_shared_empty_raw = NULL;
SEXP vctrs_shared_empty_list = NULL;
SEXP vctrs_shared_empty_date = NULL;
SEXP vctrs_shared_true = NULL;
SEXP vctrs_shared_false = NULL;

Rcomplex vctrs_shared_na_cpl;
SEXP vctrs_shared_na_lgl = NULL;
SEXP vctrs_shared_na_list = NULL;

SEXP vctrs_shared_zero_int = NULL;

SEXP strings = NULL;
SEXP strings_empty = NULL;
SEXP strings_dots = NULL;
SEXP strings_none = NULL;
SEXP strings_minimal = NULL;
SEXP strings_unique = NULL;
SEXP strings_universal = NULL;
SEXP strings_check_unique = NULL;
SEXP strings_key = NULL;
SEXP strings_loc = NULL;
SEXP strings_val = NULL;
SEXP strings_group = NULL;
SEXP strings_length = NULL;

SEXP chrs_subset = NULL;
SEXP chrs_extract = NULL;
SEXP chrs_assign = NULL;
SEXP chrs_rename = NULL;
SEXP chrs_remove = NULL;
SEXP chrs_negate = NULL;
SEXP chrs_numeric = NULL;
SEXP chrs_character = NULL;
SEXP chrs_empty = NULL;
SEXP chrs_cast = NULL;
SEXP chrs_error = NULL;

SEXP syms_i = NULL;
SEXP syms_n = NULL;
SEXP syms_x = NULL;
SEXP syms_y = NULL;
SEXP syms_to = NULL;
SEXP syms_dots = NULL;
SEXP syms_bracket = NULL;
SEXP syms_arg = NULL;
SEXP syms_x_arg = NULL;
SEXP syms_y_arg = NULL;
SEXP syms_to_arg = NULL;
SEXP syms_subscript_arg = NULL;
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

SEXP fns_bracket = NULL;
SEXP fns_quote = NULL;
SEXP fns_names = NULL;

SEXP result_attrib = NULL;

struct vctrs_arg args_empty_;


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

void vctrs_init_utils(SEXP ns) {
  vctrs_ns_env = ns;
  vctrs_method_table = r_env_get(ns, Rf_install(".__S3MethodsTable__."));

  base_method_table = r_env_get(R_BaseNamespace, Rf_install(".__S3MethodsTable__."));

  s4_c_method_table = r_parse_eval("environment(methods::getGeneric('c'))$.MTable", R_GlobalEnv);
  R_PreserveObject(s4_c_method_table);

  vctrs_shared_empty_str = Rf_mkString("");
  R_PreserveObject(vctrs_shared_empty_str);


  // Holds the CHARSXP objects because unlike symbols they can be
  // garbage collected
  strings = r_new_shared_vector(STRSXP, 21);

  strings_dots = Rf_mkChar("...");
  SET_STRING_ELT(strings, 0, strings_dots);

  strings_empty = Rf_mkChar("");
  SET_STRING_ELT(strings, 1, strings_empty);

  strings_vctrs_rcrd = Rf_mkChar("vctrs_rcrd");
  SET_STRING_ELT(strings, 2, strings_vctrs_rcrd);

  strings_date = Rf_mkChar("Date");
  SET_STRING_ELT(strings, 3, strings_date);

  strings_posixct = Rf_mkChar("POSIXct");
  SET_STRING_ELT(strings, 4, strings_posixct);

  strings_posixlt = Rf_mkChar("POSIXlt");
  SET_STRING_ELT(strings, 5, strings_posixlt);

  strings_posixt = Rf_mkChar("POSIXt");
  SET_STRING_ELT(strings, 6, strings_posixlt);

  strings_vctrs_vctr = Rf_mkChar("vctrs_vctr");
  SET_STRING_ELT(strings, 7, strings_vctrs_vctr);

  strings_none = Rf_mkChar("none");
  SET_STRING_ELT(strings, 8, strings_none);

  strings_minimal = Rf_mkChar("minimal");
  SET_STRING_ELT(strings, 9, strings_minimal);

  strings_unique = Rf_mkChar("unique");
  SET_STRING_ELT(strings, 10, strings_unique);

  strings_universal = Rf_mkChar("universal");
  SET_STRING_ELT(strings, 11, strings_universal);

  strings_check_unique = Rf_mkChar("check_unique");
  SET_STRING_ELT(strings, 12, strings_check_unique);

  strings_key = Rf_mkChar("key");
  SET_STRING_ELT(strings, 13, strings_key);

  strings_loc = Rf_mkChar("loc");
  SET_STRING_ELT(strings, 14, strings_loc);

  strings_val = Rf_mkChar("val");
  SET_STRING_ELT(strings, 15, strings_val);

  strings_group = Rf_mkChar("group");
  SET_STRING_ELT(strings, 16, strings_group);

  strings_length = Rf_mkChar("length");
  SET_STRING_ELT(strings, 17, strings_length);

  strings_factor = Rf_mkChar("factor");
  SET_STRING_ELT(strings, 18, strings_factor);

  strings_ordered = Rf_mkChar("ordered");
  SET_STRING_ELT(strings, 19, strings_ordered);

  strings_list = Rf_mkChar("list");
  SET_STRING_ELT(strings, 20, strings_list);


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
  chrs_numeric = r_new_shared_character("numeric");
  chrs_character = r_new_shared_character("character");
  chrs_empty = r_new_shared_character("");
  chrs_cast = r_new_shared_character("cast");
  chrs_error = r_new_shared_character("error");

  classes_tibble = r_new_shared_vector(STRSXP, 3);

  strings_tbl_df = Rf_mkChar("tbl_df");
  SET_STRING_ELT(classes_tibble, 0, strings_tbl_df);

  strings_tbl = Rf_mkChar("tbl");
  SET_STRING_ELT(classes_tibble, 1, strings_tbl);
  SET_STRING_ELT(classes_tibble, 2, strings_data_frame);


  classes_list_of = r_new_shared_vector(STRSXP, 3);
  strings_vctrs_list_of = Rf_mkChar("vctrs_list_of");
  SET_STRING_ELT(classes_list_of, 0, strings_vctrs_list_of);
  SET_STRING_ELT(classes_list_of, 1, strings_vctrs_vctr);
  SET_STRING_ELT(classes_list_of, 2, Rf_mkChar("list"));


  classes_vctrs_group_rle = r_new_shared_vector(STRSXP, 3);
  SET_STRING_ELT(classes_vctrs_group_rle, 0, Rf_mkChar("vctrs_group_rle"));
  SET_STRING_ELT(classes_vctrs_group_rle, 1, strings_vctrs_rcrd);
  SET_STRING_ELT(classes_vctrs_group_rle, 2, strings_vctrs_vctr);


  vctrs_shared_empty_lgl = r_new_shared_vector(LGLSXP, 0);
  vctrs_shared_empty_int = r_new_shared_vector(INTSXP, 0);
  vctrs_shared_empty_dbl = r_new_shared_vector(REALSXP, 0);
  vctrs_shared_empty_cpl = r_new_shared_vector(CPLXSXP, 0);
  vctrs_shared_empty_chr = r_new_shared_vector(STRSXP, 0);
  vctrs_shared_empty_raw = r_new_shared_vector(RAWSXP, 0);
  vctrs_shared_empty_list = r_new_shared_vector(VECSXP, 0);
  vctrs_shared_empty_date = r_new_shared_vector(REALSXP, 0);
  Rf_setAttrib(vctrs_shared_empty_date, R_ClassSymbol, classes_date);

  vctrs_shared_true = r_new_shared_vector(LGLSXP, 1);
  LOGICAL(vctrs_shared_true)[0] = 1;

  vctrs_shared_false = r_new_shared_vector(LGLSXP, 1);
  LOGICAL(vctrs_shared_false)[0] = 0;

  vctrs_shared_na_cpl.i = NA_REAL;
  vctrs_shared_na_cpl.r = NA_REAL;

  vctrs_shared_na_lgl = r_new_shared_vector(LGLSXP, 1);
  LOGICAL(vctrs_shared_na_lgl)[0] = NA_LOGICAL;

  vctrs_shared_na_list = r_new_shared_vector(VECSXP, 1);
  SET_VECTOR_ELT(vctrs_shared_na_list, 0, R_NilValue);

  vctrs_shared_zero_int = r_new_shared_vector(INTSXP, 1);
  INTEGER(vctrs_shared_zero_int)[0] = 0;

  syms_i = Rf_install("i");
  syms_n = Rf_install("n");
  syms_x = Rf_install("x");
  syms_y = Rf_install("y");
  syms_to = Rf_install("to");
  syms_dots = Rf_install("...");
  syms_bracket = Rf_install("[");
  syms_arg = Rf_install("arg");
  syms_x_arg = Rf_install("x_arg");
  syms_y_arg = Rf_install("y_arg");
  syms_to_arg = Rf_install("to_arg");
  syms_subscript_arg = Rf_install("subscript_arg");
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

  fns_bracket = Rf_findVar(syms_bracket, R_BaseEnv);
  fns_quote = Rf_findVar(Rf_install("quote"), R_BaseEnv);
  fns_names = Rf_findVar(Rf_install("names"), R_BaseEnv);

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);

  new_function_call = r_parse_eval("as.call(list(`function`, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_function_call);

  new_function__formals_node = CDR(new_function_call);
  new_function__body_node = CDR(new_function__formals_node);

  const char* formals_code = "pairlist2(... = , .x = quote(..1), .y = quote(..2), . = quote(..1))";
  rlang_formula_formals = r_parse_eval(formals_code, ns);
  R_PreserveObject(rlang_formula_formals);

  args_empty_ = new_wrapper_arg(NULL, "");

  rlang_is_splice_box = (bool (*)(SEXP)) R_GetCCallable("rlang", "rlang_is_splice_box");
  rlang_unbox = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_unbox");
  rlang_env_dots_values = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_env_dots_values");
  rlang_env_dots_list = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_env_dots_list");
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
}