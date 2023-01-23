#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/rep-decl.h"


r_obj* vec_rep(r_obj* x,
               int times,
               struct r_lazy error_call,
               struct vctrs_arg* p_x_arg,
               struct vctrs_arg* p_times_arg) {
  check_rep_times(times, error_call, p_times_arg);

  if (times == 1) {
    return x;
  }

  const r_ssize times_ = (r_ssize) times;
  const r_ssize x_size = vec_size(x);

  if (x_size == 1) {
    return vec_check_recycle(x, times_, p_x_arg, error_call);
  }

  if (multiply_would_overflow(x_size, times_)) {
    stop_rep_size_oob(error_call);
  };

  const r_ssize size = x_size * times_;

  r_obj* subscript = KEEP(r_alloc_integer(size));
  int* v_subscript = r_int_begin(subscript);

  r_ssize k = 0;

  for (r_ssize i = 0; i < times_; ++i) {
    for (r_ssize j = 1; j <= x_size; ++j, ++k) {
      v_subscript[k] = j;
    }
  }

  r_obj* out = vec_slice_unsafe(x, subscript);

  FREE(1);
  return out;
}

r_obj* ffi_vec_rep(r_obj* x, r_obj* ffi_times, r_obj* frame) {
  struct r_lazy error_call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy times_arg_lazy = { .x = syms.times_arg, .env = frame };
  struct vctrs_arg times_arg = new_lazy_arg(&times_arg_lazy);

  ffi_times = KEEP(vec_cast(ffi_times,
                            r_globals.empty_int,
                            &times_arg,
                            vec_args.empty,
                            error_call));

  if (vec_size(ffi_times) != 1) {
    stop_rep_times_size(error_call, &times_arg);
  }

  const int times = r_int_get(ffi_times, 0);
  r_obj* out = vec_rep(x, times, error_call, &x_arg, &times_arg);

  FREE(1);
  return out;
}


// -----------------------------------------------------------------------------

r_obj* vec_rep_each(r_obj* x,
                    r_obj* times,
                    struct r_lazy error_call,
                    struct vctrs_arg* p_x_arg,
                    struct vctrs_arg* p_times_arg) {
  times = KEEP(vec_cast(times,
                        r_globals.empty_int,
                        p_times_arg,
                        vec_args.empty,
                        error_call));

  const r_ssize times_size = vec_size(times);

  r_obj* out;

  if (times_size == 1) {
    const int times_ = r_int_get(times, 0);

    if (times_ == 1) {
      out = x;
    } else if (times_ == 0) {
      out = vec_slice_unsafe(x, r_globals.empty_int);
    } else {
      out = vec_rep_each_uniform(x, times_, error_call, p_times_arg);
    }
  } else {
    out = vec_rep_each_impl(x, times, times_size, error_call, p_times_arg);
  }

  FREE(1);
  return out;
}

r_obj* ffi_vec_rep_each(r_obj* x, r_obj* times, r_obj* frame) {
  struct r_lazy error_call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.times_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy times_arg_lazy = { .x = syms.times_arg, .env = frame };
  struct vctrs_arg times_arg = new_lazy_arg(&times_arg_lazy);

  return vec_rep_each(x, times, error_call, &x_arg, &times_arg);
}


// -----------------------------------------------------------------------------

static
r_obj* vec_rep_each_uniform(r_obj* x,
                            int times,
                            struct r_lazy error_call,
                            struct vctrs_arg* p_times_arg) {
  check_rep_each_times(times, 1, error_call, p_times_arg);

  const r_ssize times_ = (r_ssize) times;
  const r_ssize x_size = vec_size(x);

  if (multiply_would_overflow(x_size, times_)) {
    stop_rep_size_oob(error_call);
  };

  const r_ssize size = x_size * times_;

  r_obj* subscript = KEEP(r_alloc_integer(size));
  int* v_subscript = r_int_begin(subscript);

  r_ssize k = 0;

  for (r_ssize i = 1; i <= x_size; ++i) {
    for (r_ssize j = 0; j < times_; ++j, ++k) {
      v_subscript[k] = i;
    }
  }

  r_obj* out = vec_slice_unsafe(x, subscript);

  FREE(1);
  return out;
}

static r_obj* vec_rep_each_impl(r_obj* x,
                                r_obj* times,
                                const r_ssize times_size,
                                struct r_lazy error_call,
                                struct vctrs_arg* p_times_arg) {
  const r_ssize x_size = vec_size(x);

  if (x_size != times_size) {
    stop_recycle_incompatible_size(times_size,
                                   x_size,
                                   p_times_arg,
                                   error_call);
  }

  const int* v_times = r_int_cbegin(times);

  r_ssize size = 0;
  for (r_ssize i = 0; i < times_size; ++i) {
    const int elt_times = v_times[i];

    check_rep_each_times(elt_times, i + 1, error_call, p_times_arg);

    const r_ssize elt_times_ = (r_ssize) elt_times;

    if (plus_would_overflow(size, elt_times_)) {
      stop_rep_size_oob(error_call);
    }

    size += elt_times_;
  }

  r_obj* subscript = KEEP(r_alloc_integer(size));
  int* v_subscript = r_int_begin(subscript);

  r_ssize k = 0;

  for (r_ssize i = 1; i <= x_size; ++i) {
    const r_ssize elt_times = (r_ssize) v_times[i - 1];

    for (r_ssize j = 0; j < elt_times; ++j, ++k) {
      v_subscript[k] = i;
    }
  }

  r_obj* out = vec_slice_unsafe(x, subscript);

  FREE(1);
  return out;
}


// -----------------------------------------------------------------------------

// TODO: Modify for long vectors with `R_XLEN_T_MAX` and `R_xlen_t`.

static inline
bool times_is_oob(int times) {
  return times > R_LEN_T_MAX;
}

// Only useful for positive or zero inputs
static inline
bool multiply_would_overflow(r_ssize x, r_ssize y) {
  return (double) x * y > R_LEN_T_MAX;
}

// Only useful for positive or zero inputs
static inline
bool plus_would_overflow(r_ssize x, r_ssize y) {
  return x > R_LEN_T_MAX - y;
}


// -----------------------------------------------------------------------------

static inline
void check_rep_times(int times,
                     struct r_lazy call,
                     struct vctrs_arg* p_times_arg) {
  if (times < 0) {
    if (times == r_globals.na_int) {
      stop_rep_times_missing(call, p_times_arg);
    } else {
      stop_rep_times_negative(call, p_times_arg);
    }
  } else if (times_is_oob(times)) {
    stop_rep_times_oob(times, call, p_times_arg);
  }
}

static inline
void stop_rep_times_negative(struct r_lazy call, struct vctrs_arg* p_times_arg) {
  r_abort_lazy_call(call,
                    "%s must be a positive number.",
                    vec_arg_format(p_times_arg));
}

static inline
void stop_rep_times_missing(struct r_lazy call, struct vctrs_arg* p_times_arg) {
  r_abort_lazy_call(call,
                    "%s can't be missing.",
                    vec_arg_format(p_times_arg));
}

// Not currently thrown since `r_ssize == int`, but might be once
// long vectors are supported
static inline
void stop_rep_times_oob(int times, struct r_lazy call, struct vctrs_arg* p_times_arg) {
  r_abort_lazy_call(
    call,
    "%s must be less than %i, not %i.",
    vec_arg_format(p_times_arg),
    R_LEN_T_MAX,
    times
  );
}


// -----------------------------------------------------------------------------

static inline
void check_rep_each_times(int times,
                          r_ssize i,
                          struct r_lazy call,
                          struct vctrs_arg* p_times_arg) {
  if (times < 0) {
    if (times == r_globals.na_int) {
      stop_rep_each_times_missing(i, call, p_times_arg);
    } else {
      stop_rep_each_times_negative(i, call, p_times_arg);
    }
  } else if (times_is_oob(times)) {
    stop_rep_each_times_oob(times, i, call, p_times_arg);
  }
}

static inline
void stop_rep_each_times_negative(r_ssize i, struct r_lazy call, struct vctrs_arg* p_times_arg) {
  r_abort_lazy_call(call,
                    "%s must be a vector of positive numbers. Location %i is negative.",
                    vec_arg_format(p_times_arg),
                    i);
}

static inline
void stop_rep_each_times_missing(r_ssize i, struct r_lazy call, struct vctrs_arg* p_times_arg) {
  r_abort_lazy_call(call,
                    "%s can't be missing. Location %i is missing.",
                    vec_arg_format(p_times_arg),
                    i);
}

// Not currently thrown since `r_ssize == int`, but might be once
// long vectors are supported
static inline
void stop_rep_each_times_oob(int times, r_ssize i, struct r_lazy call, struct vctrs_arg* p_times_arg) {
  r_abort_lazy_call(
    call,
    "%s must be less than %i, not %i. ",
    "Location %i is too large.",
    vec_arg_format(p_times_arg),
    R_LEN_T_MAX,
    times,
    i
  );
}

static inline
void stop_rep_size_oob(struct r_lazy call) {
  r_abort_lazy_call(
    call,
    "Long vectors are not yet supported. "
    "Requested output size must be less than %i.",
    R_LEN_T_MAX
  );
}

static inline
void stop_rep_times_size(struct r_lazy call,
                         struct vctrs_arg* p_times_arg) {
  r_abort_lazy_call(call,
                    "%s must be a single number.",
                    vec_arg_format(p_times_arg));
}


// -----------------------------------------------------------------------------

static
r_obj* vec_unrep(r_obj* x, struct r_lazy error_call) {
  r_obj* times = KEEP(vec_run_sizes(x, error_call));
  const int* v_times = r_int_cbegin(times);

  const r_ssize size = r_length(times);

  r_obj* loc = KEEP(r_alloc_integer(size));
  int* v_loc = r_int_begin(loc);

  r_ssize current = 1;

  for (r_ssize i = 0; i < size; ++i) {
    v_loc[i] = current;
    current += v_times[i];
  }

  r_obj* out = KEEP(r_new_list(2));

  r_list_poke(out, 0, vec_slice_unsafe(x, loc));
  r_list_poke(out, 1, times);

  r_obj* names = r_new_character(2);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, strings_key);
  r_chr_poke(names, 1, strings_times);

  init_data_frame(out, size);

  FREE(3);
  return out;
}

r_obj* ffi_vec_unrep(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return vec_unrep(x, error_call);
}

// -----------------------------------------------------------------------------

void vctrs_init_rep(r_obj* ns) { }
