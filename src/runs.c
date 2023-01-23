#include "vctrs.h"

#include "decl/runs-decl.h"

// -----------------------------------------------------------------------------

r_obj* ffi_vec_detect_run_bounds(r_obj* x, r_obj* ffi_start, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  const bool start = r_arg_as_bool(ffi_start, "start");
  return vec_detect_run_bounds(x, start, error_call);
}

static
r_obj* vec_detect_run_bounds(r_obj* x, bool start, struct r_lazy error_call) {
  r_obj* where = KEEP(vec_detect_run_bounds0(x, start, error_call));
  const bool* v_where = r_raw_cbegin(where);

  const r_ssize size = r_length(where) / sizeof(bool);

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
  const bool start = r_arg_as_bool(ffi_start, "start");
  return vec_locate_run_bounds(x, start, error_call);
}

static
r_obj* vec_locate_run_bounds(r_obj* x, bool start, struct r_lazy error_call) {
  r_obj* where = KEEP(vec_detect_run_bounds0(x, start, error_call));
  const bool* v_where = r_raw_cbegin(where);

  const r_ssize size = r_length(where) / sizeof(bool);

  r_ssize n = 0;
  for (r_ssize i = 0; i < size; ++i) {
    n += v_where[i];
  }

  r_obj* out = KEEP(r_alloc_integer(n));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0, j = 0; i < size && j < n; ++i) {
    v_out[j] = i + 1;
    j += v_where[i];
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
  const bool start = true;
  r_obj* where = KEEP(vec_detect_run_bounds0(x, start, error_call));
  const bool* v_where = r_raw_cbegin(where);

  const r_ssize size = r_length(where) / sizeof(bool);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  int n = 0;

  for (r_ssize i = 0; i < size; ++i) {
    n += v_where[i];
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
  const bool start = false;
  r_obj* ends = KEEP(vec_detect_run_bounds0(x, start, error_call));
  const bool* v_ends = r_raw_cbegin(ends);

  const r_ssize size = r_length(ends) / sizeof(bool);

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
 * boolean array as a raw vector.
 */
static
r_obj* vec_detect_run_bounds0(r_obj* x, bool start, struct r_lazy error_call) {
  vec_check_vector(x, vec_args.x, error_call);

  r_obj* proxy = KEEP(vec_proxy_equal(x));
  proxy = KEEP(vec_normalize_encoding(proxy));

  const r_ssize size = vec_size(proxy);

  r_obj* out = KEEP(r_alloc_raw(size * sizeof(bool)));
  bool* v_out = r_raw_begin(out);

  const enum vctrs_type type = vec_proxy_typeof(proxy);

  switch (type) {
  case VCTRS_TYPE_logical: lgl_detect_run_bounds0(proxy, size, start, v_out); break;
  case VCTRS_TYPE_integer: int_detect_run_bounds0(proxy, size, start, v_out); break;
  case VCTRS_TYPE_double: dbl_detect_run_bounds0(proxy, size, start, v_out); break;
  case VCTRS_TYPE_complex: cpl_detect_run_bounds0(proxy, size, start, v_out); break;
  case VCTRS_TYPE_character: chr_detect_run_bounds0(proxy, size, start, v_out); break;
  case VCTRS_TYPE_raw: raw_detect_run_bounds0(proxy, size, start, v_out); break;
  case VCTRS_TYPE_list: list_detect_run_bounds0(proxy, size, start, v_out); break;
  case VCTRS_TYPE_dataframe: df_detect_run_bounds0(proxy, size, start, v_out); break;
  default: stop_unimplemented_vctrs_type("vec_detect_run_bounds0", type);
  }

  FREE(3);
  return out;
}

// -----------------------------------------------------------------------------

// Algorithm for "ends" is same as "starts", we just iterate in reverse
#define VEC_DETECT_RUN_BOUNDS0(CTYPE, CBEGIN, EQUAL_NA_EQUAL) { \
  if (size == 0) {                                              \
    /* Algorithm requires at least 1 value */                   \
    return;                                                     \
  }                                                             \
                                                                \
  CTYPE const* v_x = CBEGIN(x);                                 \
                                                                \
  if (start) {                                                  \
    /* Handle first case */                                     \
    CTYPE ref = v_x[0];                                         \
    v_out[0] = true;                                            \
                                                                \
    for (r_ssize i = 1; i < size; ++i) {                        \
      CTYPE const elt = v_x[i];                                 \
      v_out[i] = !EQUAL_NA_EQUAL(elt, ref);                     \
      ref = elt;                                                \
    }                                                           \
  } else {                                                      \
    /* Handle last case */                                      \
    CTYPE ref = v_x[size - 1];                                  \
    v_out[size - 1] = true;                                     \
                                                                \
    for (r_ssize i = size - 2; i >= 0; --i) {                   \
      CTYPE const elt = v_x[i];                                 \
      v_out[i] = !EQUAL_NA_EQUAL(elt, ref);                     \
      ref = elt;                                                \
    }                                                           \
  }                                                             \
}

static inline
void lgl_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS0(int, r_lgl_cbegin, lgl_equal_na_equal);
}
static inline
void int_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS0(int, r_int_cbegin, int_equal_na_equal);
}
static inline
void dbl_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS0(double, r_dbl_cbegin, dbl_equal_na_equal);
}
static inline
void cpl_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS0(Rcomplex, r_cpl_cbegin, cpl_equal_na_equal);
}
static inline
void chr_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS0(r_obj*, r_chr_cbegin, chr_equal_na_equal);
}
static inline
void raw_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS0(Rbyte, r_raw_cbegin, raw_equal_na_equal);
}
static inline
void list_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_DETECT_RUN_BOUNDS0(r_obj*, r_list_cbegin, list_equal_na_equal);
}

#undef VEC_DETECT_RUN_BOUNDS0

// -----------------------------------------------------------------------------

static inline
void df_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  if (size == 0) {
    // Algorithm requires at least 1 value
    return;
  }

  const r_ssize n_col = r_length(x);
  r_obj* const* v_x = r_list_cbegin(x);

  // `v_out` will eventually be `true` if we are in a run
  // continuation, and `false` if we are starting a new run.
  if (start) {
    v_out[0] = false;
    for (r_ssize i = 1; i < size; ++i) {
      v_out[i] = true;
    }
  } else {
    v_out[size - 1] = false;
    for (r_ssize i = size - 2; i >= 0; --i) {
      v_out[i] = true;
    }
  }

  for (r_ssize i = 0; i < n_col; ++i) {
    col_detect_run_bounds0(v_x[i], size, start, v_out);
  }

  // Now invert to detect the bounds
  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = !v_out[i];
  }
}

static inline
void col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: lgl_col_detect_run_bounds0(x, size, start, v_out); break;
  case VCTRS_TYPE_integer: int_col_detect_run_bounds0(x, size, start, v_out); break;
  case VCTRS_TYPE_double: dbl_col_detect_run_bounds0(x, size, start, v_out); break;
  case VCTRS_TYPE_complex: cpl_col_detect_run_bounds0(x, size, start, v_out); break;
  case VCTRS_TYPE_character: chr_col_detect_run_bounds0(x, size, start, v_out); break;
  case VCTRS_TYPE_raw: raw_col_detect_run_bounds0(x, size, start, v_out); break;
  case VCTRS_TYPE_list: list_col_detect_run_bounds0(x, size, start, v_out); break;
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened.");
  case VCTRS_TYPE_scalar: r_abort("Can't compare scalars.");
  default: r_abort("Unimplemented type.");
  }
}

#define VEC_COL_DETECT_RUN_BOUNDS0(CTYPE, CBEGIN, EQUAL_NA_EQUAL) {   \
  CTYPE const* v_x = CBEGIN(x);                                       \
                                                                      \
  if (start) {                                                        \
    CTYPE ref = v_x[0];                                               \
                                                                      \
    for (r_ssize i = 1; i < size; ++i) {                              \
      CTYPE const elt = v_x[i];                                       \
      v_out[i] = v_out[i] && EQUAL_NA_EQUAL(ref, elt);                \
      ref = elt;                                                      \
    }                                                                 \
  } else {                                                            \
    CTYPE ref = v_x[size - 1];                                        \
                                                                      \
    for (r_ssize i = size - 2; i >= 0; --i) {                         \
      CTYPE const elt = v_x[i];                                       \
      v_out[i] = v_out[i] && EQUAL_NA_EQUAL(ref, elt);                \
      ref = elt;                                                      \
    }                                                                 \
  }                                                                   \
}

static inline
void lgl_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS0(int, r_lgl_cbegin, lgl_equal_na_equal);
}
static inline
void int_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS0(int, r_int_cbegin, int_equal_na_equal);
}
static inline
void dbl_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS0(double, r_dbl_cbegin, dbl_equal_na_equal);
}
static inline
void cpl_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS0(Rcomplex, r_cpl_cbegin, cpl_equal_na_equal);
}
static inline
void chr_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS0(r_obj*, r_chr_cbegin, chr_equal_na_equal);
}
static inline
void raw_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS0(Rbyte, r_raw_cbegin, raw_equal_na_equal);
}
static inline
void list_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out) {
  VEC_COL_DETECT_RUN_BOUNDS0(r_obj*, r_list_cbegin, list_equal_na_equal);
}

#undef VEC_COL_DETECT_RUN_BOUNDS0
