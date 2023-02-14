#include "vctrs.h"
#include "decl/assert-decl.h"

void vec_assert(r_obj* x,
                r_ssize size,
                struct vctrs_arg* arg,
                struct r_lazy call) {
  vec_check_vector(x, arg, call);

  if (size != -1) {
    // `size == -1` makes no assertion about size
    vec_check_size(x, size, arg, call);
  }
}

r_obj* ffi_vec_check_vector(r_obj* x, r_obj* frame) {
  struct r_lazy call = { .x = r_syms.call, .env = frame };
  struct r_lazy arg_lazy = { .x = r_syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  vec_check_vector(x, &arg, call);

  return r_null;
}

void vec_check_vector(r_obj* x,
                      struct vctrs_arg* arg,
                      struct r_lazy call) {
  if (!obj_is_vector(x)) {
    stop_scalar_type(x, arg, call);
  }
}

r_obj* ffi_vec_check_size(r_obj* x, r_obj* ffi_size, r_obj* frame) {
  struct r_lazy call = { .x = r_syms.call, .env = frame };
  struct r_lazy arg_lazy = { .x = r_syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");

  vec_check_size(x, size, &arg, call);

  return r_null;
}

void vec_check_size(r_obj* x,
                    r_ssize size,
                    struct vctrs_arg* arg,
                    struct r_lazy call) {
  const r_ssize x_size = vec_size_3(x, arg, call);

  if (x_size != size) {
    stop_assert_size(x_size, size, arg, call);
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
  // This is an internal error
  vec_check_list(x, vec_args.x, (struct r_lazy) {.x = frame, .env = r_null });

  struct r_lazy call = { .x = r_syms.call, .env = frame };
  struct r_lazy arg_caller_data = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg_caller = new_lazy_arg(&arg_caller_data);

  r_ssize i = 0;
  struct vctrs_arg* arg = new_subscript_arg_vec(&arg_caller, x, &i);
  KEEP(arg->shelter);

  r_ssize n = r_length(x);
  r_obj* const * v_x = r_list_cbegin(x);

  for (; i < n; ++i) {
    vec_check_vector(v_x[i], arg, call);
  }

  FREE(1);
  return r_null;
}

r_obj* ffi_list_check_all_size(r_obj* xs, r_obj* ffi_size, r_obj* frame) {
  // This is an internal error
  vec_check_list(xs, vec_args.x, (struct r_lazy) {.x = frame, .env = r_null });

  struct r_lazy arg_lazy = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  struct r_lazy call = { .x = r_syms.call, .env = frame };

  r_ssize size = r_arg_as_ssize(ffi_size, "size");

  list_check_all_size(xs, size, &arg, call);

  return r_null;
}

static
void list_check_all_size(r_obj* xs,
                         r_ssize size,
                         struct vctrs_arg* p_arg,
                         struct r_lazy call) {
  if (r_typeof(xs) != R_TYPE_list) {
   r_stop_unexpected_type(r_typeof(xs));
  }

  r_ssize i = 0;

  r_ssize xs_size = r_length(xs);
  r_obj* xs_names = r_names(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  struct vctrs_arg* p_x_arg = new_subscript_arg(p_arg, xs_names, xs_size, &i);
  KEEP(p_x_arg->shelter);

  for (; i < xs_size; ++i) {
    vec_check_size(v_xs[i], size, p_x_arg, call);
  }

  FREE(1);
}
