#include "vctrs.h"
#include "decl/assert-decl.h"

void vec_assert(r_obj* x, r_ssize size, struct vctrs_arg* arg) {
  // TODO! error call
  vec_check_vector(x, arg, r_lazy_null);

  if (size != -1) {
    // `size == -1` makes no assertion about size
    vec_check_size(x, size, arg);
  }
}

void vec_check_vector(r_obj* x,
                      struct vctrs_arg* arg,
                      struct r_lazy call) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg, call);
  }
}

void vec_check_size(r_obj* x, r_ssize size, struct vctrs_arg* arg) {
  r_ssize x_size = vec_size(x);

  if (x_size != size) {
    stop_assert_size(x_size, size, arg);
  }
}


static r_no_return
void stop_non_list_type(r_obj* x,
                        struct vctrs_arg* arg,
                        struct r_lazy call) {
  r_eval_with_xyz(KEEP(r_parse("stop_non_list_type(x, y, z)")),
                  x,
                  KEEP(vctrs_arg(arg)),
                  KEEP(r_lazy_eval(call)),
                  vctrs_ns_env);
  r_stop_unreachable();
}

r_obj* ffi_check_list(r_obj* x, r_obj* frame) {
  struct r_lazy call = { .x = r_syms.call, .env = frame };
  struct r_lazy arg_data = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_data);

  vec_check_list(x, &arg, call);
  return r_null;
}

void vec_check_list(r_obj* x,
                    struct vctrs_arg* arg,
                    struct r_lazy call) {
  if (!vec_is_list(x)) {
    stop_non_list_type(x, arg, call);
  }
}


r_obj* ffi_list_check_all_vectors(r_obj* x, r_obj* frame) {
  struct r_lazy call = { .x = r_syms.call, .env = frame };

  struct r_lazy arg_caller_data = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg_caller = new_lazy_arg(&arg_caller_data);

  vec_check_list(x, &arg_caller, call);

  r_ssize i = 0;
  struct vctrs_arg* arg = new_subscript_arg(&arg_caller, x, &i);
  KEEP(arg->shelter);

  r_ssize n = r_length(x);
  r_obj* const * v_x = r_list_cbegin(x);

  for (; i < n; ++i) {
    vec_check_vector(v_x[i], arg, call);
  }

  FREE(1);
  return r_null;
}
