#include "parallel.h"
#include "assert.h"
#include "slice-assign.h"

enum list_parallel_variant {
  LIST_PARALLEL_VARIANT_all,
  LIST_PARALLEL_VARIANT_any
};

#include "decl/parallel-decl.h"

r_obj* ffi_list_pany(r_obj* ffi_xs, r_obj* ffi_missing, r_obj* ffi_size, r_obj* ffi_frame) {
  struct r_lazy xs_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  const struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const enum list_parallel_missing missing = parse_list_parallel_missing(ffi_missing, error_call);
  const r_ssize size = (ffi_size == r_null) ? -1 : r_arg_as_ssize(ffi_size, "size");

  return list_pany(ffi_xs, missing, size, &xs_arg, error_call);
}

r_obj* ffi_list_pall(r_obj* ffi_xs, r_obj* ffi_missing, r_obj* ffi_size, r_obj* ffi_frame) {
  struct r_lazy xs_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  const struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  const enum list_parallel_missing missing = parse_list_parallel_missing(ffi_missing, error_call);
  const r_ssize size = (ffi_size == r_null) ? -1 : r_arg_as_ssize(ffi_size, "size");

  return list_pall(ffi_xs, missing, size, &xs_arg, error_call);
}

r_obj* list_pany(
  r_obj* xs,
  enum list_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  return list_parallel(xs, missing, size, p_xs_arg, error_call, LIST_PARALLEL_VARIANT_any);
}

r_obj* list_pall(
  r_obj* xs,
  enum list_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
) {
  return list_parallel(xs, missing, size, p_xs_arg, error_call, LIST_PARALLEL_VARIANT_all);
}

static
r_obj* list_parallel(
  r_obj* xs,
  enum list_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call,
  enum list_parallel_variant parallel
) {
  // Input must be a list
  obj_check_list(xs, p_xs_arg, error_call);

  // Every element of that list must be a bare logical vector
  list_check_all_condition_indices(xs, p_xs_arg, error_call);

  // Every element of that list must be the same size
  size = compute_size(size, xs);
  list_check_all_size(xs, size, VCTRS_ALLOW_NULL_no, p_xs_arg, error_call);

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  const r_ssize xs_size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  if (xs_size == 0) {
    // Zero input case is special, fill with values that match `any()` and `all()`
    switch (parallel) {
      case LIST_PARALLEL_VARIANT_all: r_p_lgl_fill(v_out, 1, size); break;
      case LIST_PARALLEL_VARIANT_any: r_p_lgl_fill(v_out, 0, size); break;
      default: r_stop_unreachable();
    }
  } else {
    // Initialize output with first input
    r_obj* x = v_xs[0];
    const int* v_x = r_lgl_begin(x);
    list_parallel_init(v_x, missing, size, v_out);

    // Combine with remaining inputs
    for (r_ssize i = 1; i < xs_size; ++i) {
      r_obj* x = v_xs[i];
      const int* v_x = r_lgl_begin(x);

      switch (parallel) {
        case LIST_PARALLEL_VARIANT_all: list_pall_fill(v_x, missing, size, v_out); break;
        case LIST_PARALLEL_VARIANT_any: list_pany_fill(v_x, missing, size, v_out); break;
        default: r_stop_unreachable();
      }
    }
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

// Same, regardless of variant
static inline
void list_parallel_init(const int* v_x, enum list_parallel_missing missing, r_ssize size, int* v_out) {
  switch (missing) {
    case LIST_PARALLEL_MISSING_na: list_parallel_init_missing_as_na(v_x, size, v_out); break;
    case LIST_PARALLEL_MISSING_false: list_parallel_init_missing_as_false(v_x, size, v_out); break;
    case LIST_PARALLEL_MISSING_true: list_parallel_init_missing_as_true(v_x, size, v_out); break;
    default: r_stop_unreachable();
  }
}

// Propagates `NA`
static inline
void list_parallel_init_missing_as_na(const int* v_x, r_ssize size, int* v_out) {
  r_memcpy(v_out, v_x, sizeof(*v_out) * size);
}

// Turns `NA` into `FALSE`
static inline
void list_parallel_init_missing_as_false(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];
    v_out[i] = (elt != r_globals.na_lgl) * elt;
  }
}

// Turns `NA` into `TRUE`
static inline
void list_parallel_init_missing_as_true(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = (bool) v_x[i];
  }
}

// -----------------------------------------------------------------------------

static inline
void list_pany_fill(const int* v_x, enum list_parallel_missing missing, r_ssize size, int* v_out) {
  switch (missing) {
    case LIST_PARALLEL_MISSING_na: list_pany_fill_missing_as_na(v_x, size, v_out); break;
    case LIST_PARALLEL_MISSING_false: list_pany_fill_missing_as_false(v_x, size, v_out); break;
    case LIST_PARALLEL_MISSING_true: list_pany_fill_missing_as_true(v_x, size, v_out); break;
    default: r_stop_unreachable();
  }
}

static inline
void list_pall_fill(const int* v_x, enum list_parallel_missing missing, r_ssize size, int* v_out) {
  switch (missing) {
    case LIST_PARALLEL_MISSING_na: list_pall_fill_missing_as_na(v_x, size, v_out); break;
    case LIST_PARALLEL_MISSING_false: list_pall_fill_missing_as_false(v_x, size, v_out); break;
    case LIST_PARALLEL_MISSING_true: list_pall_fill_missing_as_true(v_x, size, v_out); break;
    default: r_stop_unreachable();
  }
}

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
 * `list_pall_fill_*()` and `list_pany_fill_*()` are symmetrical.
 *
 * A nice property of these implementations is that they don't rely on
 * assumptions about two's complement, bitwise operations, or the underlying
 * value of `NA_LOGICAL` in any way, making them as portable as possible.
 */

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
void list_pany_fill_missing_as_na(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];

    const bool any_true = (elt_out == 1) || (elt_x == 1);
    const bool equal = elt_out == elt_x;
    v_out[i] = any_true + !any_true * (equal * elt_out + !equal * r_globals.na_lgl);
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
void list_pall_fill_missing_as_na(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];

    const bool any_false = !elt_out || !elt_x;
    const bool equal = elt_out == elt_x;
    v_out[i] = !any_false * (equal * elt_out + !equal * r_globals.na_lgl);
  }
}

/*
 * Never need to worry about `N || *`, because the initialization loop
 * turns the first input's `N`s into `F`s.
 *
 * Treat `N == F`
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
void list_pany_fill_missing_as_false(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];
    v_out[i] = elt_out || (elt_x == 1);
  }
}

/*
 * Never need to worry about `N && *`, because the initialization loop
 * turns the first input's `N`s into `F`s.
 *
 * Treat `N == F`
 *
 * F && F == F
 * F && T == F
 * F && N == F
 *
 * T && F == F
 * T && T == T
 * T && N == F
 */
static inline
void list_pall_fill_missing_as_false(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];
    v_out[i] = elt_out && (elt_x == 1);
  }
}

/*
 * Never need to worry about `N || *`, because the initialization loop
 * turns the first input's `N`s into `T`s.
 *
 * Treat `N == T`
 *
 * F || F == F
 * F || T == T
 * F || N == T
 *
 * T || F == T
 * T || T == T
 * T || N == T
 */
static inline
void list_pany_fill_missing_as_true(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];
    v_out[i] = elt_out || elt_x;
  }
}

/*
 * Never need to worry about `N && *`, because the initialization loop
 * turns the first input's `N`s into `T`s.
 *
 * Treat `N == T`
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
void list_pall_fill_missing_as_true(const int* v_x, r_ssize size, int* v_out) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt_out = v_out[i];
    const int elt_x = v_x[i];
    v_out[i] = elt_out && elt_x;
  }
}

// -----------------------------------------------------------------------------

static
enum list_parallel_missing parse_list_parallel_missing(r_obj* missing, struct r_lazy error_call) {
  if (missing == r_null) {
    return LIST_PARALLEL_MISSING_na;
  }

  if (r_is_bool(missing)) {
    if (r_lgl_get(missing, 0)) {
      return LIST_PARALLEL_MISSING_true;
    } else {
      return LIST_PARALLEL_MISSING_false;
    }
  }

  r_abort_lazy_call(error_call, "`missing` must be `NULL`, `TRUE`, or `FALSE`.");
}

// Figure out the output size
// - `size` if supplied
// - Size of 1st `conditions` element if one exists
// - Size 0 if `conditions` is empty
static
r_ssize compute_size(r_ssize size, r_obj* xs) {
  if (size != -1) {
    return size;
  }

  if (r_length(xs) == 0) {
    return 0;
  }

  return r_length(r_list_get(xs, 0));
}
