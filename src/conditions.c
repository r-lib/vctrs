#include "vctrs.h"
#include "utils.h"


void stop_scalar_type(SEXP x, struct vctrs_arg* arg) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_scalar_type"),
                               PROTECT(r_protect(x)),
                               PROTECT(vctrs_arg(arg))));
  Rf_eval(call, vctrs_ns_env);
  Rf_error("Internal error: `stop_scalar_type()` should have jumped earlier");
}

void vec_assert(SEXP x, struct vctrs_arg* arg) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg);
  }
}

void stop_incompatible_size(SEXP x, SEXP y,
                            R_len_t x_size, R_len_t y_size,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg) {
  SEXP syms[7] = {
    syms_x,
    syms_y,
    r_sym("x_size"),
    r_sym("y_size"),
    syms_x_arg,
    syms_y_arg,
    NULL
  };
  SEXP args[7] = {
    PROTECT(r_protect(x)),
    PROTECT(r_protect(y)),
    PROTECT(r_int(x_size)),
    PROTECT(r_int(y_size)),
    PROTECT(vctrs_arg(x_arg)),
    PROTECT(vctrs_arg(y_arg)),
    NULL
  };

  SEXP call = PROTECT(r_call(r_sym("stop_incompatible_size"), syms, args));
  Rf_eval(call, vctrs_ns_env);

  Rf_error("Internal error: `stop_incompatible_size()` should have jumped earlier");
}

void stop_recycle_incompatible_size(R_len_t x_size, R_len_t size,
                                    struct vctrs_arg* x_arg) {
  SEXP syms[4] = {
    r_sym("x_size"),
    r_sym("size"),
    r_sym("x_arg"),
    NULL
  };
  SEXP args[4] = {
    PROTECT(r_int(x_size)),
    PROTECT(r_int(size)),
    PROTECT(vctrs_arg(x_arg)),
    NULL
  };

  SEXP call = PROTECT(r_call(r_sym("stop_recycle_incompatible_size"), syms, args));
  Rf_eval(call, vctrs_ns_env);

  Rf_error("Internal error: `stop_recycle_incompatible_size()` should have jumped earlier");
}
