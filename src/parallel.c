#include "vctrs.h"

enum vctrs_parallel {
  VCTRS_PARALLEL_all = 0,
  VCTRS_PARALLEL_any = 1
};

#include "decl/parallel-decl.h"

// -----------------------------------------------------------------------------

r_obj* ffi_vec_pall(r_obj* xs, r_obj* ffi_na_rm, r_obj* ffi_size, r_obj* frame) {
  return ffi_vec_p(xs, ffi_na_rm, ffi_size, frame, VCTRS_PARALLEL_all);
}
r_obj* ffi_vec_pany(r_obj* xs, r_obj* ffi_na_rm, r_obj* ffi_size, r_obj* frame) {
  return ffi_vec_p(xs, ffi_na_rm, ffi_size, frame, VCTRS_PARALLEL_any);
}

static inline
r_obj* ffi_vec_p(r_obj* xs,
                 r_obj* ffi_na_rm,
                 r_obj* ffi_size,
                 r_obj* frame,
                 enum vctrs_parallel parallel) {
  struct r_lazy call = { .x = frame, .env = r_null };

  const bool na_rm = r_arg_as_bool(ffi_na_rm, ".na_rm");

  r_ssize size = -1;
  if (ffi_size == r_null) {
    size = vec_check_size_common(xs, 0, vec_args.empty, call);
  } else {
    size = vec_as_short_length(ffi_size, vec_args.dot_size, call);
  }

  return vec_p(xs, na_rm, size, parallel, call);
}

// -----------------------------------------------------------------------------

static
r_obj* vec_p(r_obj* xs,
             bool na_rm,
             r_ssize size,
             enum vctrs_parallel parallel,
             struct r_lazy call) {
  xs = KEEP(vec_cast_common(xs, r_globals.empty_lgl, vec_args.empty, call));

  const struct size_common_opts recycle_opts = {
    .p_arg = vec_args.empty,
    .call = call
  };
  xs = KEEP(vec_recycle_common_opts(xs, size, &recycle_opts));

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  if (n == 0) {
    switch (parallel) {
    case VCTRS_PARALLEL_all: r_p_lgl_fill(v_out, 1, size); break;
    case VCTRS_PARALLEL_any: r_p_lgl_fill(v_out, 0, size); break;
    }
  } else {
    r_obj* x = v_xs[0];
    const int* v_x = r_lgl_begin(x);

    switch (parallel) {
    case VCTRS_PARALLEL_all: vec_pall_init(v_x, na_rm, size, v_out); break;
    case VCTRS_PARALLEL_any: vec_pany_init(v_x, na_rm, size, v_out); break;
    }
  }

  for (r_ssize i = 1; i < n; ++i) {
    r_obj* x = v_xs[i];
    const int* v_x = r_lgl_begin(x);

    switch (parallel) {
    case VCTRS_PARALLEL_all: vec_pall_fill(v_x, na_rm, size, v_out); break;
    case VCTRS_PARALLEL_any: vec_pany_fill(v_x, na_rm, size, v_out); break;
    }
  }

  FREE(3);
  return out;
}

// -----------------------------------------------------------------------------

static inline
void vec_pall_init(const int* v_x, bool na_rm, r_ssize size, int* v_out) {
  if (na_rm) {
    vec_pall_init_na_rm(v_x, size, v_out);
  } else {
    vec_pall_init_na_keep(v_x, size, v_out);
  }
}
static inline
void vec_pany_init(const int* v_x, bool na_rm, r_ssize size, int* v_out) {
  if (na_rm) {
    vec_pany_init_na_rm(v_x, size, v_out);
  } else {
    vec_pany_init_na_keep(v_x, size, v_out);
  }
}

static inline
void vec_pall_fill(const int* v_x, bool na_rm, r_ssize size, int* v_out) {
  if (na_rm) {
    vec_pall_fill_na_rm(v_x, size, v_out);
  } else {
    vec_pall_fill_na_keep(v_x, size, v_out);
  }
}
static inline
void vec_pany_fill(const int* v_x, bool na_rm, r_ssize size, int* v_out) {
  if (na_rm) {
    vec_pany_fill_na_rm(v_x, size, v_out);
  } else {
    vec_pany_fill_na_keep(v_x, size, v_out);
  }
}

// -----------------------------------------------------------------------------

/*
 * Each of these implementations has been highly optimized to be completely
 * branchless. Additionally, we are careful to ensure that the access of both
 * `v_out[i]` and `v_x[i]` is mandatory at each iteration rather than
 * conditional (i.e. `v_out[i] && v_x[i]` vs `elt_out && elt_x`). Conditional
 * access of `v_x[i]` in particular can destroy performance here, as it prevents
 * the compiler from heavily optimizing the actual computation.
 *
 * Additionally, the implementations of pall/pany have been designed to be as
 * symmetrical as possible to increase code clarity. For example,
 * `vec_pall_fill_na_rm()` and `vec_pany_fill_na_rm()` are symmetrical, as are
 * the two fill variants of `*_na_keep()`.
 *
 * A nice property of these implementations is that they don't rely on
 * assumptions about two's complement, bitwise operations, or the underlying
 * value of `NA_LOGICAL` in any way, making them as portable as possible.
 */

/*
 * Never need to worry about `N && *`, because the initialization takes care
 * of missing values in the first input, and they are never propagated after
 * that.
 *
 * F && F == F
 * F && T == F
 * F && N == F
 *
 * T && F == F
 * T && T == T
 * T && N == T
 */
static inline
void vec_pall_init_na_rm(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = (bool) v_x[i];
  }
}
static inline
void vec_pall_fill_na_rm(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];
    v_out[i] = elt_out && elt_x;
  }
}

/*
 * Never need to worry about `N || *`, because the initialization takes care
 * of missing values in the first input, and they are never propagated after
 * that.
 *
 * F || F == F
 * F || T == T
 * F || N == F
 *
 * T || F == T
 * T || T == T
 * T || N == T
 */
static inline
void vec_pany_init_na_rm(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];
    v_out[i] = (elt != r_globals.na_lgl) * elt;
  }
}
static inline
void vec_pany_fill_na_rm(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];
    v_out[i] = (elt_out == 1) || (elt_x == 1);
  }
}

/*
 * F && F == F
 * F && T == F
 * F && N == F
 *
 * T && F == F
 * T && T == T
 * T && N == N
 *
 * N && F == F
 * N && T == N
 * N && N == N
 */
static inline
void vec_pall_init_na_keep(const int* v_x, r_ssize size, int* v_out) {
  memcpy(v_out, v_x, sizeof(*v_out) * size);
}
static inline
void vec_pall_fill_na_keep(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];

    const bool any_false = !elt_out || !elt_x;
    const bool equal = elt_out == elt_x;
    v_out[i] = !any_false * (equal * elt_out + !equal * r_globals.na_lgl);
  }
}

/*
 * F || F == F
 * F || T == T
 * F || N == N
 *
 * T || F == T
 * T || T == T
 * T || N == T
 *
 * N || F == N
 * N || T == T
 * N || N == N
 */
static inline
void vec_pany_init_na_keep(const int* v_x, r_ssize size, int* v_out) {
  memcpy(v_out, v_x, sizeof(*v_out) * size);
}
static inline
void vec_pany_fill_na_keep(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];

    const bool any_true = (elt_out == 1) || (elt_x == 1);
    const bool equal = elt_out == elt_x;
    v_out[i] = any_true + !any_true * (equal * elt_out + !equal * r_globals.na_lgl);
  }
}
