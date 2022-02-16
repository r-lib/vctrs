#ifndef VCTRS_RLANG_DEV_H
#define VCTRS_RLANG_DEV_H

#include <rlang.h>

struct r_lazy {
  r_obj* x;
  r_obj* env;
};

static inline
r_obj* r_lazy_eval(struct r_lazy lazy) {
  if (!lazy.env) {
    // Unitialised lazy variable
    return r_null;
  } else if (lazy.env == r_null) {
    // Forced lazy variable
    return lazy.x;
  } else {
    return r_eval(lazy.x, lazy.env);
  }
}

extern
struct r_lazy r_lazy_null;

static inline
r_obj* r_lazy_eval_protect(struct r_lazy lazy) {
  r_obj* out = KEEP(r_lazy_eval(lazy));
  out = r_expr_protect(out);

  FREE(1);
  return out;
}

#define r_abort_lazy_call(LAZY, ...) \
  r_abort_call(KEEP(r_lazy_eval(LAZY)), __VA_ARGS__)


static inline
const char* r_c_str_format_error_arg(const char* x) {
  r_obj* ffi_x = KEEP(r_chr(x));
  const char* out = r_format_error_arg(ffi_x);
  FREE(1);
  return out;
}


#endif
