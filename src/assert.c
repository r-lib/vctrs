#include "vctrs.h"

r_obj* ffi_obj_is_vector(r_obj* x) {
  // Not exposed at the R level for single vector checks
  const enum vctrs_allow_null allow_null = VCTRS_ALLOW_NULL_no;
  return r_lgl(obj_is_vector(x, allow_null));
}

r_obj* ffi_obj_check_vector(r_obj* x, r_obj* frame) {
  struct r_lazy call = { .x = r_syms.call, .env = frame };

  struct r_lazy arg_lazy = { .x = r_syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  // Not exposed at the R level for single vector checks
  const enum vctrs_allow_null allow_null = VCTRS_ALLOW_NULL_no;

  obj_check_vector(x, allow_null, &arg, call);

  return r_null;
}

r_obj* ffi_list_all_vectors(r_obj* ffi_xs, r_obj* ffi_allow_null, r_obj* ffi_frame) {
  obj_check_list(ffi_xs, vec_args.x, (struct r_lazy) { ffi_frame, r_null });
  const enum vctrs_allow_null allow_null = arg_as_allow_null(ffi_allow_null, "allow_null");
  return r_lgl(list_all_vectors(ffi_xs, allow_null));
}

bool list_all_vectors(r_obj* xs, enum vctrs_allow_null allow_null) {
  if (r_typeof(xs) != R_TYPE_list) {
    r_stop_unexpected_type(r_typeof(xs));
  }

  const r_ssize size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* x = v_xs[i];
    if (!obj_is_vector(x, allow_null)) {
      return false;
    }
  }

  return true;
}

r_obj* ffi_list_check_all_vectors(r_obj* ffi_xs, r_obj* ffi_allow_null, r_obj* ffi_frame) {
  // This is an internal error
  obj_check_list(ffi_xs, vec_args.x, (struct r_lazy) {.x = ffi_frame, .env = r_null });

  struct r_lazy call = { .x = r_syms.call, .env = ffi_frame };

  struct r_lazy xs_arg_lazy = { .x = syms.arg, .env = ffi_frame };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  const enum vctrs_allow_null allow_null = arg_as_allow_null(ffi_allow_null, "allow_null");

  list_check_all_vectors(ffi_xs, allow_null, &xs_arg, call);

  return r_null;
}

void list_check_all_vectors(
  r_obj* xs,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
) {
  r_ssize i = 0;
  struct vctrs_arg* x_arg = new_subscript_arg_vec(p_xs_arg, xs, &i);
  KEEP(x_arg->shelter);

  const r_ssize xs_size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  for (; i < xs_size; ++i) {
    r_obj* x = v_xs[i];
    obj_check_vector(x, allow_null, x_arg, call);
  }

  FREE(1);
}

r_obj* ffi_vec_check_size(r_obj* x, r_obj* ffi_size, r_obj* frame) {
  struct r_lazy call = { .x = r_syms.call, .env = frame };
  struct r_lazy arg_lazy = { .x = r_syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");

  // Not exposed at the R level for single vector checks
  const enum vctrs_allow_null allow_null = VCTRS_ALLOW_NULL_no;

  vec_check_size(x, size, allow_null, &arg, call);

  return r_null;
}

r_obj* ffi_list_all_size(
  r_obj* ffi_xs,
  r_obj* ffi_size,
  r_obj* ffi_allow_null,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null };

  // This is an internal error
  obj_check_list(ffi_xs, vec_args.x, error_call);

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");
  const enum vctrs_allow_null allow_null = arg_as_allow_null(ffi_allow_null, "allow_null");

  return r_lgl(list_all_size(ffi_xs, size, allow_null, vec_args.x, error_call));
}

bool list_all_size(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
) {
  if (r_typeof(xs) != R_TYPE_list) {
    r_stop_unexpected_type(r_typeof(xs));
  }

  r_ssize i = 0;

  const r_ssize xs_size = r_length(xs);
  r_obj* xs_names = r_names(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  struct vctrs_arg* p_x_arg = new_subscript_arg(p_xs_arg, xs_names, xs_size, &i);
  KEEP(p_x_arg->shelter);

  bool out = true;

  for (; i < xs_size; ++i) {
    r_obj* x = v_xs[i];
    if (!vec_is_size(x, size, allow_null, p_x_arg, call)) {
      out = false;
      break;
    }
  }

  FREE(1);
  return out;
}

r_obj* ffi_list_check_all_size(
  r_obj* ffi_xs,
  r_obj* ffi_size,
  r_obj* ffi_allow_null,
  r_obj* ffi_frame
) {
  // This is an internal error
  obj_check_list(ffi_xs, vec_args.x, (struct r_lazy) {.x = ffi_frame, .env = r_null });

  struct r_lazy xs_arg_lazy = { .x = syms.arg, .env = ffi_frame };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  struct r_lazy call = { .x = r_syms.call, .env = ffi_frame };

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");
  const enum vctrs_allow_null allow_null = arg_as_allow_null(ffi_allow_null, "allow_null");

  list_check_all_size(ffi_xs, size, allow_null, &xs_arg, call);

  return r_null;
}

void list_check_all_size(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
) {
  if (r_typeof(xs) != R_TYPE_list) {
   r_stop_unexpected_type(r_typeof(xs));
  }

  r_ssize i = 0;

  const r_ssize xs_size = r_length(xs);
  r_obj* xs_names = r_names(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  struct vctrs_arg* p_x_arg = new_subscript_arg(p_xs_arg, xs_names, xs_size, &i);
  KEEP(p_x_arg->shelter);

  for (; i < xs_size; ++i) {
    vec_check_size(v_xs[i], size, allow_null, p_x_arg, call);
  }

  FREE(1);
}

r_obj* ffi_obj_is_list(r_obj* x) {
  return r_lgl(obj_is_list(x));
}

r_no_return
void stop_non_list_type(
  r_obj* x,
  struct vctrs_arg* arg,
  struct r_lazy call
) {
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

  obj_check_list(x, &arg, call);
  return r_null;
}

r_obj* ffi_vec_check_recyclable(r_obj* x, r_obj* ffi_size, r_obj* frame) {
  struct r_lazy call = { .x = r_syms.call, .env = frame };
  struct r_lazy arg_lazy = { .x = r_syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_lazy);

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");

  // Not exposed at the R level for single vector checks
  const enum vctrs_allow_null allow_null = VCTRS_ALLOW_NULL_no;

  vec_check_recyclable(x, size, allow_null, &arg, call);

  return r_null;
}

r_obj* ffi_list_all_recyclable(
  r_obj* ffi_xs,
  r_obj* ffi_size,
  r_obj* ffi_allow_null,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null };

  // This is an internal error
  obj_check_list(ffi_xs, vec_args.x, error_call);

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");
  const enum vctrs_allow_null allow_null = arg_as_allow_null(ffi_allow_null, "allow_null");

  return r_lgl(list_all_recyclable(ffi_xs, size, allow_null, vec_args.x, error_call));
}

bool list_all_recyclable(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
) {
  if (r_typeof(xs) != R_TYPE_list) {
    r_stop_unexpected_type(r_typeof(xs));
  }

  r_ssize i = 0;

  const r_ssize xs_size = r_length(xs);
  r_obj* xs_names = r_names(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  struct vctrs_arg* p_x_arg = new_subscript_arg(p_xs_arg, xs_names, xs_size, &i);
  KEEP(p_x_arg->shelter);

  bool out = true;

  for (; i < xs_size; ++i) {
    r_obj* x = v_xs[i];
    if (!vec_is_recyclable(x, size, allow_null, p_x_arg, call)) {
      out = false;
      break;
    }
  }

  FREE(1);
  return out;
}

r_obj* ffi_list_check_all_recyclable(
  r_obj* ffi_xs,
  r_obj* ffi_size,
  r_obj* ffi_allow_null,
  r_obj* ffi_frame
) {
  // This is an internal error
  obj_check_list(ffi_xs, vec_args.x, (struct r_lazy) {.x = ffi_frame, .env = r_null });

  struct r_lazy xs_arg_lazy = { .x = syms.arg, .env = ffi_frame };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  struct r_lazy call = { .x = r_syms.call, .env = ffi_frame };

  const r_ssize size = r_arg_as_ssize(ffi_size, "size");
  const enum vctrs_allow_null allow_null = arg_as_allow_null(ffi_allow_null, "allow_null");

  list_check_all_recyclable(ffi_xs, size, allow_null, &xs_arg, call);

  return r_null;
}

void list_check_all_recyclable(
  r_obj* xs,
  r_ssize size,
  enum vctrs_allow_null allow_null,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy call
) {
  if (r_typeof(xs) != R_TYPE_list) {
    r_stop_unexpected_type(r_typeof(xs));
  }

  r_ssize i = 0;

  const r_ssize xs_size = r_length(xs);
  r_obj* xs_names = r_names(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  struct vctrs_arg* p_x_arg = new_subscript_arg(p_xs_arg, xs_names, xs_size, &i);
  KEEP(p_x_arg->shelter);

  for (; i < xs_size; ++i) {
    vec_check_recyclable(v_xs[i], size, allow_null, p_x_arg, call);
  }

  FREE(1);
}
