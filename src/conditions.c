#include "vctrs.h"
#include "utils.h"


struct vctrs_arg new_vctrs_arg(const char* arg) {
  struct vctrs_arg wrapper = { .data = (const void* ) arg, .get = NULL };
  return wrapper;
}
const char* vctrs_arg(struct vctrs_arg* arg) {
  if (arg->get) {
    return arg->get(arg);
  } else {
    return (const char*) arg->data;
  }
}

struct vctrs_arg args_empty = { .data = (const void*) "", .get = NULL};


void stop_scalar_type(SEXP x, struct vctrs_arg* arg) {
  const char* arg_str = vctrs_arg(arg);

  SEXP arg_chr;
  if (strlen(arg_str)) {
    arg_chr = PROTECT(Rf_mkString(arg_str));
  } else {
    arg_chr = PROTECT(R_NilValue);
  }

  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_scalar_type"),
                               r_protect(x),
                               arg_chr));
  Rf_eval(call, vctrs_ns_env);
  Rf_error("Internal error: `stop_scalar_type()` should have jumped");
}

void vec_assert(SEXP x, struct vctrs_arg* arg) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg);
  }
}
