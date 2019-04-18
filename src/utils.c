#include "vctrs.h"
#include "utils.h"

#include <R_ext/Rdynload.h>

// Initialised at load time
bool (*rlang_is_splice_box)(SEXP) = NULL;
SEXP (*rlang_unbox)(SEXP) = NULL;
SEXP (*rlang_env_dots_values)(SEXP) = NULL;


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

  SEXP dispatch_call = PROTECT(r_call(fn_sym, syms, args));

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

SEXP with_proxy(SEXP x, SEXP (*rec)(SEXP, bool), SEXP i) {
  SEXP proxy = PROTECT(vec_proxy(x));

  SEXP out = PROTECT(rec(proxy, false));
  out = vec_restore(out, x, i);

  UNPROTECT(2);
  return out;
}

bool is_compact_rownames(SEXP x) {
  return Rf_length(x) == 2 && INTEGER(x)[0] == NA_INTEGER;
}
R_len_t compact_rownames_length(SEXP x) {
  return abs(INTEGER(x)[1]);
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


SEXP vctrs_ns_env = NULL;

SEXP syms_i = NULL;
SEXP syms_x = NULL;
SEXP syms_y = NULL;
SEXP syms_to = NULL;
SEXP syms_dots = NULL;
SEXP syms_bracket = NULL;

SEXP fns_bracket = NULL;
SEXP fns_quote = NULL;

void vctrs_init_utils(SEXP ns) {
  vctrs_ns_env = ns;

  syms_i = Rf_install("i");
  syms_x = Rf_install("x");
  syms_y = Rf_install("y");
  syms_to = Rf_install("to");
  syms_dots = Rf_install("...");
  syms_bracket = Rf_install("[");

  fns_bracket = Rf_findVar(syms_bracket, R_BaseEnv);
  fns_quote = Rf_findVar(Rf_install("quote"), R_BaseEnv);

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);

  rlang_is_splice_box = (bool (*)(SEXP)) R_GetCCallable("rlang", "rlang_is_splice_box");
  rlang_unbox = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_unbox");
  rlang_env_dots_values = (SEXP (*)(SEXP)) R_GetCCallable("rlang", "rlang_env_dots_values");
}
