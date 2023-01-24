#include "vctrs.h"
#include "vec-bool.h"

enum vctrs_run_bound {
  VCTRS_RUN_BOUND_start = 0,
  VCTRS_RUN_BOUND_end = 1
};

#include "decl/runs-decl.h"

// -----------------------------------------------------------------------------

r_obj* ffi_vec_detect_run_bounds(r_obj* x, r_obj* ffi_start, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  const enum vctrs_run_bound which = as_run_bound(ffi_start, error_call);
  return vec_detect_run_bounds(x, which, error_call);
}

static
r_obj* vec_detect_run_bounds(r_obj* x, enum vctrs_run_bound which, struct r_lazy error_call) {
  struct r_bool_vector* p_where = vec_detect_run_bounds_bool(x, which, error_call);
  KEEP(p_where->shelter);
  const bool* v_where = r_bool_vector_cbegin(p_where);

  const r_ssize size = r_bool_vector_length(p_where);

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = v_where[i];
  }

  FREE(2);
  return out;
}

// -----------------------------------------------------------------------------

r_obj* ffi_vec_locate_run_bounds(r_obj* x, r_obj* ffi_start, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  const enum vctrs_run_bound which = as_run_bound(ffi_start, error_call);
  return vec_locate_run_bounds(x, which, error_call);
}

static
r_obj* vec_locate_run_bounds(r_obj* x, enum vctrs_run_bound which, struct r_lazy error_call) {
  struct r_bool_vector* p_where = vec_detect_run_bounds_bool(x, which, error_call);
  KEEP(p_where->shelter);
  const bool* v_where = r_bool_vector_cbegin(p_where);

  const r_ssize size = r_bool_vector_length(p_where);

  r_ssize n = 0;
  for (r_ssize i = 0; i < size; ++i) {
    n += v_where[i];
  }

  r_obj* out = KEEP(r_alloc_integer(n));
  int* v_out = r_int_begin(out);
  r_ssize j = compute_iter_loc(n, which);

  r_ssize loc = compute_iter_loc(size, which);
  const r_ssize step = compute_iter_step(which);

  // First/last value are always the final bound locations
  // (depending on `which`), so `j` won't ever write to OOB locations
  for (r_ssize i = 0; i < size; ++i) {
    v_out[j] = loc + 1;
    j += step * v_where[loc];
    loc += step;
  }

  FREE(2);
  return out;
}

// -----------------------------------------------------------------------------

r_obj* ffi_vec_identify_runs(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return vec_identify_runs(x, error_call);
}

r_obj* vec_identify_runs(r_obj* x, struct r_lazy error_call) {
  struct r_bool_vector* p_starts = vec_detect_run_bounds_bool(x, VCTRS_RUN_BOUND_start, error_call);
  KEEP(p_starts->shelter);
  const bool* v_starts = r_bool_vector_cbegin(p_starts);

  const r_ssize size = r_bool_vector_length(p_starts);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  int n = 0;

  for (r_ssize i = 0; i < size; ++i) {
    n += v_starts[i];
    v_out[i] = n;
  }

  r_obj* ffi_n = r_int(n);
  r_attrib_poke(out, syms_n, ffi_n);

  FREE(2);
  return out;
}

// -----------------------------------------------------------------------------

r_obj* ffi_vec_run_sizes(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return vec_run_sizes(x, error_call);
}

r_obj* vec_run_sizes(r_obj* x, struct r_lazy error_call) {
  struct r_bool_vector* p_ends = vec_detect_run_bounds_bool(x, VCTRS_RUN_BOUND_end, error_call);
  KEEP(p_ends->shelter);
  const bool* v_ends = r_bool_vector_cbegin(p_ends);

  const r_ssize size = r_bool_vector_length(p_ends);

  r_ssize n = 0;
  for (r_ssize i = 0; i < size; ++i) {
    n += v_ends[i];
  }

  r_obj* out = KEEP(r_alloc_integer(n));
  int* v_out = r_int_begin(out);
  r_ssize j = 0;

  int count = 1;

  for (r_ssize i = 0; i < size; ++i) {
    const bool end = v_ends[i];
    v_out[j] = count;
    j += end;
    count = !end * count + 1;
  }

  FREE(2);
  return out;
}

// -----------------------------------------------------------------------------

/*
 * Like `vec_detect_run_bounds()`, but returns a less memory intensive
 * boolean array as an `r_bool_vector`.
 */
static
struct r_bool_vector* vec_detect_run_bounds_bool(r_obj* x,
                                                 enum vctrs_run_bound which,
                                                 struct r_lazy error_call) {
  vec_check_vector(x, vec_args.x, error_call);

  r_obj* proxy = KEEP(vec_proxy_equal(x));
  proxy = KEEP(vec_normalize_encoding(proxy));

  const r_ssize size = vec_size(proxy);

  struct r_bool_vector* p_out = r_new_bool_vector(size);
  KEEP(p_out->shelter);
  bool* v_out = r_bool_vector_begin(p_out);

  const enum vctrs_type type = vec_proxy_typeof(proxy);

  switch (type) {
  case VCTRS_TYPE_logical: lgl_detect_run_bounds_bool(proxy, size, which, v_out); break;
  case VCTRS_TYPE_integer: int_detect_run_bounds_bool(proxy, size, which, v_out); break;
  case VCTRS_TYPE_double: dbl_detect_run_bounds_bool(proxy, size, which, v_out); break;
  case VCTRS_TYPE_complex: cpl_detect_run_bounds_bool(proxy, size, which, v_out); break;
  case VCTRS_TYPE_character: chr_detect_run_bounds_bool(proxy, size, which, v_out); break;
  case VCTRS_TYPE_raw: raw_detect_run_bounds_bool(proxy, size, which, v_out); break;
  case VCTRS_TYPE_list: list_detect_run_bounds_bool(proxy, size, which, v_out); break;
  case VCTRS_TYPE_dataframe: df_detect_run_bounds_bool(proxy, size, which, v_out); break;
  default: stop_unimplemented_vctrs_type("vec_detect_run_bounds_bool", type);
  }

  FREE(3);
  return p_out;
}

// -----------------------------------------------------------------------------

// Algorithm for "ends" is same as "starts", we just iterate in reverse
#define VEC_DETECT_RUN_BOUNDS_BOOL(CTYPE, CBEGIN, EQUAL_NA_EQUAL) { \
  if (size == 0) {                                                  \
    /* Algorithm requires at least 1 value */                       \
    return;                                                         \
  }                                                                 \
                                                                    \
  CTYPE const* v_x = CBEGIN(x);                                     \
                                                                    \
  r_ssize loc = compute_iter_loc(size, which);                      \
  const r_ssize step = compute_iter_step(which);                    \
                                                                    \
  /* Handle first/last value */                                     \
  CTYPE ref = v_x[loc];                                             \
  v_out[loc] = true;                                                \
  loc += step;                                                      \
                                                                    \
  for (r_ssize i = 1; i < size; ++i) {                              \
    CTYPE const elt = v_x[loc];                                     \
    v_out[loc] = !EQUAL_NA_EQUAL(elt, ref);                         \
    ref = elt;                                                      \
    loc += step;                                                    \
  }                                                                 \
}

static inline
void lgl_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS_BOOL(int, r_lgl_cbegin, lgl_equal_na_equal);
}
static inline
void int_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS_BOOL(int, r_int_cbegin, int_equal_na_equal);
}
static inline
void dbl_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS_BOOL(double, r_dbl_cbegin, dbl_equal_na_equal);
}
static inline
void cpl_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS_BOOL(Rcomplex, r_cpl_cbegin, cpl_equal_na_equal);
}
static inline
void chr_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS_BOOL(r_obj*, r_chr_cbegin, chr_equal_na_equal);
}
static inline
void raw_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS_BOOL(Rbyte, r_raw_cbegin, raw_equal_na_equal);
}
static inline
void list_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS_BOOL(r_obj*, r_list_cbegin, list_equal_na_equal);
}

#undef VEC_DETECT_RUN_BOUNDS_BOOL

// -----------------------------------------------------------------------------

static inline
void df_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  if (size == 0) {
    // Algorithm requires at least 1 value
    return;
  }

  const r_ssize n_col = r_length(x);
  r_obj* const* v_x = r_list_cbegin(x);

  r_ssize loc = compute_iter_loc(size, which);
  const r_ssize step = compute_iter_step(which);

  // `v_out` will eventually be `true` if we are in a run
  // continuation, and `false` if we are starting a new run.
  v_out[loc] = false;
  loc += step;

  for (r_ssize i = 1; i < size; ++i) {
    v_out[loc] = true;
    loc += step;
  }

  for (r_ssize i = 0; i < n_col; ++i) {
    col_detect_run_bounds_bool(v_x[i], size, which, v_out);
  }

  // Now invert to detect the bounds
  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = !v_out[i];
  }
}

static inline
void col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: lgl_col_detect_run_bounds_bool(x, size, which, v_out); break;
  case VCTRS_TYPE_integer: int_col_detect_run_bounds_bool(x, size, which, v_out); break;
  case VCTRS_TYPE_double: dbl_col_detect_run_bounds_bool(x, size, which, v_out); break;
  case VCTRS_TYPE_complex: cpl_col_detect_run_bounds_bool(x, size, which, v_out); break;
  case VCTRS_TYPE_character: chr_col_detect_run_bounds_bool(x, size, which, v_out); break;
  case VCTRS_TYPE_raw: raw_col_detect_run_bounds_bool(x, size, which, v_out); break;
  case VCTRS_TYPE_list: list_col_detect_run_bounds_bool(x, size, which, v_out); break;
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened.");
  case VCTRS_TYPE_scalar: r_abort("Can't compare scalars.");
  default: r_abort("Unimplemented type.");
  }
}

#define VEC_COL_DETECT_RUN_BOUNDS_BOOL(CTYPE, CBEGIN, EQUAL_NA_EQUAL) {   \
  CTYPE const* v_x = CBEGIN(x);                                           \
                                                                          \
  r_ssize loc = compute_iter_loc(size, which);                            \
  const r_ssize step = compute_iter_step(which);                          \
                                                                          \
  CTYPE ref = v_x[loc];                                                   \
  loc += step;                                                            \
                                                                          \
  for (r_ssize i = 1; i < size; ++i) {                                    \
    CTYPE const elt = v_x[loc];                                           \
    v_out[loc] = v_out[loc] && EQUAL_NA_EQUAL(ref, elt);                  \
    ref = elt;                                                            \
    loc += step;                                                          \
  }                                                                       \
}

static inline
void lgl_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS_BOOL(int, r_lgl_cbegin, lgl_equal_na_equal);
}
static inline
void int_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS_BOOL(int, r_int_cbegin, int_equal_na_equal);
}
static inline
void dbl_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS_BOOL(double, r_dbl_cbegin, dbl_equal_na_equal);
}
static inline
void cpl_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS_BOOL(Rcomplex, r_cpl_cbegin, cpl_equal_na_equal);
}
static inline
void chr_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS_BOOL(r_obj*, r_chr_cbegin, chr_equal_na_equal);
}
static inline
void raw_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS_BOOL(Rbyte, r_raw_cbegin, raw_equal_na_equal);
}
static inline
void list_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS_BOOL(r_obj*, r_list_cbegin, list_equal_na_equal);
}

#undef VEC_COL_DETECT_RUN_BOUNDS_BOOL

// -----------------------------------------------------------------------------

static inline
r_ssize compute_iter_loc(r_ssize size, enum vctrs_run_bound which) {
  switch (which) {
  case VCTRS_RUN_BOUND_start: return 0;
  case VCTRS_RUN_BOUND_end: return size - 1;
  default: r_stop_internal("Unknown `which` value.");
  }
}

static inline
r_ssize compute_iter_step(enum vctrs_run_bound which) {
  switch (which) {
  case VCTRS_RUN_BOUND_start: return 1;
  case VCTRS_RUN_BOUND_end: return -1;
  default: r_stop_internal("Unknown `which` value.");
  }
}

static inline
enum vctrs_run_bound as_run_bound(r_obj* which, struct r_lazy error_call) {
  struct r_lazy error_arg = { .x = chrs_which, .env = r_null };

  r_obj* values = KEEP(r_alloc_character(2));
  r_chr_poke(values, 0, r_str("start"));
  r_chr_poke(values, 1, r_str("end"));

  const int match = r_arg_match(which, values, error_arg, error_call);

  enum vctrs_run_bound out;

  switch (match) {
  case 0: out = VCTRS_RUN_BOUND_start; break;
  case 1: out = VCTRS_RUN_BOUND_end; break;
  default: r_stop_internal("Unknown `which` value.");
  }

  FREE(1);
  return out;
}
