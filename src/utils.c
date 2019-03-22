#include "vctrs.h"
#include "utils.h"

bool is_bool(SEXP x) {
  return
    TYPEOF(x) == LGLSXP &&
    Rf_length(x) == 1 &&
    *LOGICAL(x) != NA_LOGICAL;
}

/**
 * Dispatch with two arguments
 *
 * @param fn The method to call.
 * @param x,y Arguments passed to the method.
 * @param fn_sym,x_sym,y_sym Symbols to which `x` and `y` should be
 *   assigned.  The assignment occurs in `env` and the dispatch call
 *   refers to these symbols.
 * @param env The environment in which to dispatch. Should be the
 *   global environment or inherit from it so methods defined there
 *   are picked up. If the global environment, a child is created so
 *   the call components can be masked.
 *
 *   If `env` contains dots, the dispatch call forwards dots.
 */
SEXP vctrs_dispatch2(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y) {
  // Create a child so we can mask the call components
  SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 3));

  // Forward new values in the dispatch environment
  Rf_defineVar(fn_sym, fn, env);
  Rf_defineVar(x_sym, x, env);
  Rf_defineVar(y_sym, y, env);

  // Forward dots to methods if they exist
  SEXP dispatch_call;
  if (Rf_findVar(syms_dots, env) == R_UnboundValue) {
    dispatch_call = PROTECT(Rf_lang3(fn, x, y));
  } else {
    dispatch_call = PROTECT(Rf_lang4(fn, x, y, syms_dots));
  }

  SEXP out = Rf_eval(dispatch_call, env);

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


SEXP syms_i = NULL;
SEXP syms_x = NULL;
SEXP syms_y = NULL;
SEXP syms_to = NULL;
SEXP syms_dots = NULL;

void vctrs_init_utils(SEXP ns) {
  syms_i = Rf_install("i");
  syms_x = Rf_install("x");
  syms_y = Rf_install("y");
  syms_to = Rf_install("to");
  syms_dots = Rf_install("...");

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);
}
