#include "vctrs.h"
#include "utils.h"

#include <R_ext/Rdynload.h>

// Initialised at load time
bool (*rlang_is_splice_box)(SEXP) = NULL;
SEXP (*rlang_unbox)(SEXP) = NULL;
SEXP (*rlang_env_dots_values)(SEXP) = NULL;
SEXP (*rlang_env_dots_list)(SEXP) = NULL;
SEXP vctrs_method_table = NULL;

SEXP strings_tbl = NULL;
SEXP strings_tbl_df = NULL;
SEXP strings_data_frame = NULL;
SEXP strings_vctrs_rcrd = NULL;
SEXP strings_posixt = NULL;
SEXP strings_posixlt = NULL;
SEXP strings_vctrs_vctr = NULL;

SEXP classes_data_frame = NULL;
SEXP classes_tibble = NULL;

static SEXP syms_as_list = NULL;
static SEXP syms_as_data_frame = NULL;
static SEXP fns_as_list = NULL;
static SEXP fns_as_data_frame = NULL;


bool is_bool(SEXP x) {
  return
    TYPEOF(x) == LGLSXP &&
    Rf_length(x) == 1 &&
    LOGICAL(x)[0] != NA_LOGICAL;
}

/**
 * Dispatch with two arguments
 *
 * @param fn The method to call.
 * @param syms A null-terminated array of symbols. The arguments `args`
 *   are assigned to these symbols. The assignment occurs in `env` and
 *   the dispatch call refers to these symbols.
 * @param args A null-terminated array of arguments passed to the method.
 * @param env The environment in which to dispatch. Should be the
 *   global environment or inherit from it so methods defined there
 *   are picked up. If the global environment, a child is created so
 *   the call components can be masked.
 *
 *   If `env` contains dots, the dispatch call forwards dots.
 */
SEXP vctrs_dispatch_n(SEXP fn_sym, SEXP fn, SEXP* syms, SEXP* args) {
  // Create a child so we can mask the call components
  SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 4));

  // Forward new values in the dispatch environment
  Rf_defineVar(fn_sym, fn, env);

  SEXP dispatch_call = PROTECT(r_call(fn_sym, syms, syms));

  while (*syms) {
    Rf_defineVar(*syms, *args, env);
    ++syms; ++args;
  }

  SEXP out = Rf_eval(dispatch_call, env);

  UNPROTECT(2);
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

SEXP df_map(SEXP df, SEXP (*fn)(SEXP)) {
  R_len_t n = Rf_length(df);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    SET_VECTOR_ELT(out, i, fn(VECTOR_ELT(df, i)));
  }

  // FIXME: Should that be restored?
  SEXP nms = PROTECT(Rf_getAttrib(df, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  out = df_restore(out, df, vctrs_shared_empty_int);

  UNPROTECT(2);
  return out;
}

inline void never_reached(const char* fn) {
  Rf_error("Internal error in `%s()`: Never reached", fn);
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
static SEXP s3_get_method(const char* generic, const char* class) {
  SEXP sym = s3_method_sym(generic, class);

  SEXP method = r_env_get(R_GlobalEnv, sym);
  if (r_is_function(method)) {
    return method;
  }

  method = r_env_get(vctrs_method_table, sym);
  if (r_is_function(method)) {
    return method;
  }

  return R_NilValue;
}

SEXP s3_find_method(const char* generic, SEXP x) {
  if (!OBJECT(x)) {
    return R_NilValue;
  }

  SEXP class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  SEXP* class_ptr = STRING_PTR(class);
  int n_class = Rf_length(class);

  for (int i = 0; i < n_class; ++i, ++class_ptr) {
    SEXP method = s3_get_method(generic, CHAR(*class_ptr));
    if (method != R_NilValue) {
      UNPROTECT(1);
      return method;
    }
  }

  UNPROTECT(1);
  return R_NilValue;
}


// Initialised at load time
SEXP compact_seq_attrib = NULL;

// Returns a compact sequence that `vec_slice()` understands
SEXP compact_seq(R_len_t from, R_len_t to) {
  SEXP seq = PROTECT(Rf_allocVector(INTSXP, 2));

  int* p = INTEGER(seq);
  p[0] = from;
  p[1] = to;

  SET_ATTRIB(seq, compact_seq_attrib);

  UNPROTECT(1);
  return seq;
}
bool is_compact_seq(SEXP x) {
  return ATTRIB(x) == compact_seq_attrib;
}


// From rlang
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
        *which_data = NA_INTEGER;
      } else {
        *which_data = i + 1;
      }
      ++which_data;
    }
  }

  UNPROTECT(1);
  return which;
}


#define FILL(CTYPE, DEREF)                      \
  R_len_t n = Rf_length(x);                     \
  CTYPE* data = DEREF(x);                       \
                                                \
  for (R_len_t i = 0; i < n; ++i, ++data)       \
    *data = value

void r_lgl_fill(SEXP x, int value) {
  FILL(int, LOGICAL);
}
void r_int_fill(SEXP x, int value) {
  FILL(int, INTEGER);
}

#undef FILL


void r_int_fill_seq(SEXP x, int start) {
  R_len_t n = Rf_length(x);
  int* data = INTEGER(x);

  for (R_len_t i = 0; i < n; ++i, ++data, ++start) {
    *data = start;
  }
}

SEXP r_seq(R_len_t from, R_len_t to) {
  R_len_t n = to - from;
  SEXP seq = PROTECT(Rf_allocVector(INTSXP, n));

  r_int_fill_seq(seq, from);

  UNPROTECT(1);
  return seq;
}


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

// [[ include("utils.h") ]]
SEXP r_protect(SEXP x) {
  return Rf_lang2(fns_quote, x);
}

bool r_is_bool(SEXP x) {
  return TYPEOF(x) == LGLSXP &&
    Rf_length(x) == 1 &&
    LOGICAL(x)[0] != NA_LOGICAL;
}
bool r_is_true(SEXP x) {
  return r_is_bool(x) && LOGICAL(x)[0] == 1;
}

bool r_is_string(SEXP x) {
  return TYPEOF(x) == STRSXP &&
    Rf_length(x) == 1 &&
    STRING_ELT(x, 0) != NA_STRING;
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

SEXP r_names(SEXP x) {
  return Rf_getAttrib(x, R_NamesSymbol);
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
  return elt != NA_STRING && elt != Rf_mkChar("");
}

SEXP r_env_get(SEXP env, SEXP sym) {
  SEXP obj = Rf_findVarInFrame3(env, sym, FALSE);

  // Force lazy loaded bindings
  if (TYPEOF(obj) == PROMSXP) {
    obj = Rf_eval(obj, R_BaseEnv);
  }

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

bool r_chr_has_string(SEXP x, SEXP str) {
  int n = Rf_length(x);
  SEXP* data = STRING_PTR(x);

  for (int i = 0; i < n; ++i, ++data) {
    if (*data == str) {
      return true;
    }
  }

  return false;
}

SEXP r_as_list(SEXP x) {
  if (OBJECT(x)) {
    return vctrs_dispatch1(syms_as_list, fns_as_list, syms_x, x);
  } else {
    return Rf_coerceVector(x, VECSXP);
  }
}
SEXP r_as_data_frame(SEXP x) {
  if (is_bare_data_frame(x)) {
    return x;
  } else {
    return vctrs_dispatch1(syms_as_data_frame, fns_as_data_frame, syms_x, x);
  }
}


SEXP vctrs_ns_env = NULL;
SEXP vctrs_shared_empty_str = NULL;

SEXP vctrs_shared_empty_lgl = NULL;
SEXP vctrs_shared_empty_int = NULL;
SEXP vctrs_shared_empty_dbl = NULL;
SEXP vctrs_shared_empty_cpl = NULL;
SEXP vctrs_shared_empty_chr = NULL;
SEXP vctrs_shared_empty_raw = NULL;
SEXP vctrs_shared_empty_list = NULL;
SEXP vctrs_shared_true = NULL;
SEXP vctrs_shared_false = NULL;
Rcomplex vctrs_shared_na_cpl;

SEXP strings = NULL;
SEXP strings_empty = NULL;
SEXP strings_dots = NULL;

SEXP syms_i = NULL;
SEXP syms_x = NULL;
SEXP syms_y = NULL;
SEXP syms_to = NULL;
SEXP syms_dots = NULL;
SEXP syms_bracket = NULL;
SEXP syms_x_arg = NULL;
SEXP syms_y_arg = NULL;
SEXP syms_out = NULL;
SEXP syms_value = NULL;

SEXP fns_bracket = NULL;
SEXP fns_quote = NULL;
SEXP fns_names = NULL;

struct vctrs_arg args_empty_;
struct vctrs_arg* args_empty = NULL;

void vctrs_init_utils(SEXP ns) {
  vctrs_ns_env = ns;
  vctrs_method_table = r_env_get(ns, Rf_install(".__S3MethodsTable__."));

  vctrs_shared_empty_str = Rf_mkString("");
  R_PreserveObject(vctrs_shared_empty_str);


  // Holds the CHARSXP objects because unlike symbols they can be
  // garbage collected
  strings = Rf_allocVector(STRSXP, 6);
  R_PreserveObject(strings);

  strings_dots = Rf_mkChar("...");
  SET_STRING_ELT(strings, 0, strings_dots);

  strings_empty = Rf_mkChar("");
  SET_STRING_ELT(strings, 1, strings_empty);

  strings_vctrs_rcrd = Rf_mkChar("vctrs_rcrd");
  SET_STRING_ELT(strings, 2, strings_vctrs_rcrd);

  strings_posixlt = Rf_mkChar("POSIXlt");
  SET_STRING_ELT(strings, 3, strings_posixlt);

  strings_posixt = Rf_mkChar("POSIXt");
  SET_STRING_ELT(strings, 4, strings_posixlt);

  strings_vctrs_vctr = Rf_mkChar("vctrs_vctr");
  SET_STRING_ELT(strings, 5, strings_vctrs_vctr);


  classes_data_frame = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(classes_data_frame);

  strings_data_frame = Rf_mkChar("data.frame");
  SET_STRING_ELT(classes_data_frame, 0, strings_data_frame);


  classes_tibble = Rf_allocVector(STRSXP, 3);
  R_PreserveObject(classes_tibble);

  strings_tbl_df = Rf_mkChar("tbl_df");
  SET_STRING_ELT(classes_tibble, 0, strings_tbl_df);

  strings_tbl = Rf_mkChar("tbl");
  SET_STRING_ELT(classes_tibble, 1, strings_tbl);

  SET_STRING_ELT(classes_tibble, 2, strings_data_frame);


  vctrs_shared_empty_lgl = Rf_allocVector(LGLSXP, 0);
  R_PreserveObject(vctrs_shared_empty_lgl);
  MARK_NOT_MUTABLE(vctrs_shared_empty_lgl);

  vctrs_shared_empty_int = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(vctrs_shared_empty_int);
  MARK_NOT_MUTABLE(vctrs_shared_empty_int);

  vctrs_shared_empty_dbl = Rf_allocVector(REALSXP, 0);
  R_PreserveObject(vctrs_shared_empty_dbl);
  MARK_NOT_MUTABLE(vctrs_shared_empty_dbl);

  vctrs_shared_empty_cpl = Rf_allocVector(CPLXSXP, 0);
  R_PreserveObject(vctrs_shared_empty_cpl);
  MARK_NOT_MUTABLE(vctrs_shared_empty_cpl);

  vctrs_shared_empty_chr = Rf_allocVector(STRSXP, 0);
  R_PreserveObject(vctrs_shared_empty_chr);
  MARK_NOT_MUTABLE(vctrs_shared_empty_chr);

  vctrs_shared_empty_raw = Rf_allocVector(RAWSXP, 0);
  R_PreserveObject(vctrs_shared_empty_raw);
  MARK_NOT_MUTABLE(vctrs_shared_empty_raw);

  vctrs_shared_empty_list = Rf_allocVector(VECSXP, 0);
  R_PreserveObject(vctrs_shared_empty_list);
  MARK_NOT_MUTABLE(vctrs_shared_empty_list);

  vctrs_shared_true = Rf_allocVector(LGLSXP, 1);
  R_PreserveObject(vctrs_shared_true);
  MARK_NOT_MUTABLE(vctrs_shared_true);
  LOGICAL(vctrs_shared_true)[0] = 1;

  vctrs_shared_false = Rf_allocVector(LGLSXP, 1);
  R_PreserveObject(vctrs_shared_false);
  MARK_NOT_MUTABLE(vctrs_shared_false);
  LOGICAL(vctrs_shared_false)[0] = 0;

  vctrs_shared_na_cpl.i = NA_REAL;
  vctrs_shared_na_cpl.r = NA_REAL;


  syms_i = Rf_install("i");
  syms_x = Rf_install("x");
  syms_y = Rf_install("y");
  syms_to = Rf_install("to");
  syms_dots = Rf_install("...");
  syms_bracket = Rf_install("[");
  syms_x_arg = Rf_install("x_arg");
  syms_y_arg = Rf_install("y_arg");
  syms_out = Rf_install("out");
  syms_value = Rf_install("value");

  fns_bracket = Rf_findVar(syms_bracket, R_BaseEnv);
  fns_quote = Rf_findVar(Rf_install("quote"), R_BaseEnv);
  fns_names = Rf_findVar(Rf_install("names"), R_BaseEnv);

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);

  args_empty_ = new_wrapper_arg(NULL, "");
  args_empty = &args_empty_;

  rlang_is_splice_box = (bool (*)(SEXP)) R_GetCCallable("rlang", "rlang_is_splice_box");
  rlang_unbox = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_unbox");
  rlang_env_dots_values = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_env_dots_values");
  rlang_env_dots_list = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_env_dots_list");

  syms_as_list = Rf_install("as.list");
  syms_as_data_frame = Rf_install("as.data.frame");
  fns_as_list = r_env_get(R_BaseEnv, syms_as_list);
  fns_as_data_frame = r_env_get(R_BaseEnv, syms_as_data_frame);


  compact_seq_attrib = Rf_cons(R_NilValue, R_NilValue);
  R_PreserveObject(compact_seq_attrib);
  SET_TAG(compact_seq_attrib, Rf_install("vctrs_compact_seq"));
}
