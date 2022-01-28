#ifndef VCTRS_RLANG_DEV_H
#define VCTRS_RLANG_DEV_H


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


#endif
