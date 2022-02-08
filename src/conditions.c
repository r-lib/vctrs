#include "vctrs.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
void stop_scalar_type(r_obj* x,
                      struct vctrs_arg* arg,
                      struct r_lazy call) {
  r_obj* ffi_call = KEEP(r_lazy_eval(call));
  ffi_call = KEEP(r_expr_protect(ffi_call));

  r_obj* stop_call = KEEP(r_call4(r_sym("stop_scalar_type"),
                                  KEEP(r_protect(x)),
                                  KEEP(vctrs_arg(arg)),
                                  ffi_call));
  r_eval(stop_call, vctrs_ns_env);
  r_stop_unreachable();
}

// [[ include("vctrs.h") ]]
void stop_assert_size(r_ssize actual,
                      r_ssize required,
                      struct vctrs_arg* arg) {
  r_obj* syms[4] = {
   syms_actual,
   syms_required,
   syms_arg,
   NULL
  };
  r_obj* args[4] = {
    KEEP(r_int(actual)),
    KEEP(r_int(required)),
    KEEP(vctrs_arg(arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_assert_size, syms, args));
  r_eval(call, vctrs_ns_env);

  never_reached("stop_assert_size");
}

// [[ include("vctrs.h") ]]
void stop_incompatible_type(SEXP x,
                            SEXP y,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg,
                            bool cast) {
  SEXP syms[6] = {
    syms_x,
    syms_y,
    syms_x_arg,
    syms_y_arg,
    syms_action,
    NULL
  };
  SEXP args[6] = {
    PROTECT(r_protect(x)),
    PROTECT(r_protect(y)),
    PROTECT(vctrs_arg(x_arg)),
    PROTECT(vctrs_arg(y_arg)),
    cast ? chrs_convert : chrs_combine,
    NULL
  };

  SEXP call = PROTECT(r_call_n(syms_stop_incompatible_type, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_incompatible_type");
}

// [[ include("vctrs.h") ]]
void stop_incompatible_size(SEXP x, SEXP y,
                            R_len_t x_size, R_len_t y_size,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg) {
  SEXP syms[7] = {
    syms_x,
    syms_y,
    syms_x_size,
    syms_y_size,
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

  SEXP call = PROTECT(r_call_n(syms_stop_incompatible_size, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_incompatible_size");
}

void stop_recycle_incompatible_size(r_ssize x_size,
                                    r_ssize size,
                                    struct vctrs_arg* x_arg,
                                    struct r_lazy call) {
  r_obj* syms[5] = {
    r_sym("x_size"),
    r_sym("size"),
    r_sym("x_arg"),
    syms_call,
    NULL
  };
  r_obj* args[5] = {
    KEEP(r_int(x_size)),
    KEEP(r_int(size)),
    KEEP(vctrs_arg(x_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* stop_call = KEEP(r_call_n(r_sym("stop_recycle_incompatible_size"), syms, args));
  r_eval(stop_call, vctrs_ns_env);

  never_reached("stop_recycle_incompatible_size");
}

void stop_incompatible_shape(SEXP x, SEXP y,
                             R_len_t x_size, R_len_t y_size, int axis,
                             struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg) {
  SEXP syms[8] = {
    r_sym("x"),
    r_sym("y"),
    r_sym("x_size"),
    r_sym("y_size"),
    r_sym("axis"),
    r_sym("x_arg"),
    r_sym("y_arg"),
    NULL
  };
  SEXP args[8] = {
    PROTECT(r_protect(x)),
    PROTECT(r_protect(y)),
    PROTECT(r_int(x_size)),
    PROTECT(r_int(y_size)),
    PROTECT(r_int(axis)),
    PROTECT(vctrs_arg(p_x_arg)),
    PROTECT(vctrs_arg(p_y_arg)),
    NULL
  };

  SEXP call = PROTECT(r_call_n(r_sym("stop_incompatible_shape"), syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_incompatible_shape");
}

void stop_corrupt_factor_levels(SEXP x, struct vctrs_arg* arg) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_corrupt_factor_levels"),
                               PROTECT(r_protect(x)),
                               PROTECT(vctrs_arg(arg))));
  Rf_eval(call, vctrs_ns_env);
  never_reached("stop_corrupt_factor_levels");
}

void stop_corrupt_ordered_levels(SEXP x, struct vctrs_arg* arg) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_corrupt_ordered_levels"),
                               PROTECT(r_protect(x)),
                               PROTECT(vctrs_arg(arg))));
  Rf_eval(call, vctrs_ns_env);
  never_reached("stop_corrupt_ordered_levels");
}
