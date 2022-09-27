#include "vctrs.h"

#include "decl/runs-decl.h"

// -----------------------------------------------------------------------------

r_obj* vctrs_locate_runs(r_obj* x, r_obj* ffi_start) {
  const bool start = r_arg_as_bool(ffi_start, "start");
  return vec_locate_runs(x, start);
}

static
r_obj* vec_locate_runs(r_obj* x, bool start) {
  r_obj* id = KEEP(vec_identify_runs(x));
  const int* v_id = r_int_cbegin(id);

  const r_ssize size = r_length(id);
  const int n = r_int_get(r_attrib_get(id, syms_n), 0);

  // Share memory with `id`.
  // `vec_locate_run_starts/ends()` are carefully written to avoid
  // overwrite issues.
  r_obj* out = id;
  int* v_out = r_int_begin(out);

  if (start) {
    vec_locate_run_starts(v_id, size, v_out);
  } else {
    vec_locate_run_ends(v_id, size, v_out);
  }

  // Resize shared memory to output size and clear attribute
  out = KEEP(r_int_resize(out, n));
  r_attrib_poke(out, syms_n, r_null);

  FREE(2);
  return out;
}

static inline
void vec_locate_run_starts(const int* v_id, r_ssize size, int* v_out) {
  if (size == 0) {
    return;
  }

  r_ssize loc = 0;
  int ref = v_id[0];

  // Handle first case
  v_out[loc] = 1;
  ++loc;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];
    v_out[loc] = i + 1;
    loc += elt != ref;
    ref = elt;
  }
}

static inline
void vec_locate_run_ends(const int* v_id, r_ssize size, int* v_out) {
  if (size == 0) {
    return;
  }

  r_ssize loc = 0;
  int ref = v_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];
    v_out[loc] = i;
    loc += elt != ref;
    ref = elt;
  }

  // Handle last case
  v_out[loc] = size;
}

// -----------------------------------------------------------------------------

r_obj* vctrs_detect_runs(r_obj* x, r_obj* ffi_start) {
  bool start = r_arg_as_bool(ffi_start, "start");
  return vec_detect_runs(x, start);
}

static
r_obj* vec_detect_runs(r_obj* x, bool start) {
  r_obj* id = KEEP(vec_identify_runs(x));
  const int* v_id = r_int_cbegin(id);

  r_ssize size = r_length(id);

  r_obj* out = KEEP(r_new_logical(size));
  int* v_out = r_lgl_begin(out);

  if (start) {
    vec_detect_run_starts(v_id, size, v_out);
  } else {
    vec_detect_run_ends(v_id, size, v_out);
  }

  FREE(2);
  return out;
}

static inline
void vec_detect_run_starts(const int* v_id, r_ssize size, int* v_out) {
  if (size == 0) {
    return;
  }

  int ref = v_id[0];

  // Handle first case
  v_out[0] = 1;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];
    v_out[i] = elt != ref;
    ref = elt;
  }
}

static inline
void vec_detect_run_ends(const int* v_id, r_ssize size, int* v_out) {
  if (size == 0) {
    return;
  }

  int ref = v_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];
    v_out[i - 1] = elt != ref;
    ref = elt;
  }

  // Handle last case
  v_out[size - 1] = 1;
}

// -----------------------------------------------------------------------------

r_obj* vctrs_identify_runs(r_obj* x) {
  return vec_identify_runs(x);
}

r_obj* vec_identify_runs(r_obj* x) {
  r_obj* proxy = KEEP(vec_proxy_equal(x));
  r_ssize size = vec_size(proxy);
  proxy = KEEP(vec_normalize_encoding(proxy));

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  // Handle size 0 up front.
  // All implementations assume at least 1 element.
  if (size == 0) {
    r_obj* ffi_n = r_int(0);
    r_attrib_poke(out, syms_n, ffi_n);
    FREE(3);
    return out;
  }

  const enum vctrs_type type = vec_proxy_typeof(proxy);

  int n;

  switch (type) {
  case VCTRS_TYPE_logical: n = lgl_identify_runs(proxy, size, v_out); break;
  case VCTRS_TYPE_integer: n = int_identify_runs(proxy, size, v_out); break;
  case VCTRS_TYPE_double: n = dbl_identify_runs(proxy, size, v_out); break;
  case VCTRS_TYPE_complex: n = cpl_identify_runs(proxy, size, v_out); break;
  case VCTRS_TYPE_character: n = chr_identify_runs(proxy, size, v_out); break;
  case VCTRS_TYPE_raw: n = raw_identify_runs(proxy, size, v_out); break;
  case VCTRS_TYPE_list: n = list_identify_runs(proxy, size, v_out); break;
  case VCTRS_TYPE_dataframe: n = df_identify_runs(proxy, size, v_out); break;
  default: stop_unimplemented_vctrs_type("vec_identify_runs", type);
  }

  r_obj* ffi_n = r_int(n);
  r_attrib_poke(out, syms_n, ffi_n);

  FREE(3);
  return out;
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS(CTYPE, CBEGIN, EQUAL_NA_EQUAL) {       \
  int id = 1;                                                    \
  CTYPE const* v_x = CBEGIN(x);                                  \
                                                                 \
  /* Handle first case */                                        \
  CTYPE ref = v_x[0];                                            \
  v_out[0] = id;                                                 \
                                                                 \
  for (r_ssize i = 1; i < size; ++i) {                           \
    CTYPE const elt = v_x[i];                                    \
    id += !EQUAL_NA_EQUAL(elt, ref);                             \
    v_out[i] = id;                                               \
    ref = elt;                                                   \
  }                                                              \
                                                                 \
  return id;                                                     \
}

static
int lgl_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  VEC_IDENTIFY_RUNS(int, r_lgl_cbegin, lgl_equal_na_equal);
}
static
int int_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  VEC_IDENTIFY_RUNS(int, r_int_cbegin, int_equal_na_equal);
}
static
int dbl_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  VEC_IDENTIFY_RUNS(double, r_dbl_cbegin, dbl_equal_na_equal);
}
static
int cpl_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  VEC_IDENTIFY_RUNS(Rcomplex, r_cpl_cbegin, cpl_equal_na_equal);
}
static
int chr_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  VEC_IDENTIFY_RUNS(r_obj*, r_chr_cbegin, chr_equal_na_equal);
}
static
int raw_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  VEC_IDENTIFY_RUNS(Rbyte, r_raw_cbegin, raw_equal_na_equal);
}
static
int list_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  VEC_IDENTIFY_RUNS(r_obj*, r_list_cbegin, list_equal_na_equal);
}

#undef VEC_IDENTIFY_RUNS

// -----------------------------------------------------------------------------

static inline
int df_identify_runs(r_obj* x, r_ssize size, int* v_out) {
  const r_ssize n_col = r_length(x);
  r_obj* const* v_x = r_list_cbegin(x);

  // Boolean vector that will eventually be `true` if we are in a run
  // continuation, and `false` if we are starting a new run.
  r_obj* where_shelter = KEEP(r_alloc_raw(size * sizeof(bool)));
  bool* v_where = (bool*) r_raw_begin(where_shelter);

  v_where[0] = false;
  for (r_ssize i = 1; i < size; ++i) {
    v_where[i] = true;
  }

  for (r_ssize i = 0; i < n_col; ++i) {
    col_identify_runs(v_x[i], size, v_where);
  }

  int id = 1;

  v_out[0] = id;
  for (r_ssize i = 1; i < size; ++i) {
    id += !v_where[i];
    v_out[i] = id;
  }

  FREE(1);
  return id;
}

static inline
void col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: lgl_col_identify_runs(x, size, v_where); break;
  case VCTRS_TYPE_integer: int_col_identify_runs(x, size, v_where); break;
  case VCTRS_TYPE_double: dbl_col_identify_runs(x, size, v_where); break;
  case VCTRS_TYPE_complex: cpl_col_identify_runs(x, size, v_where); break;
  case VCTRS_TYPE_character: chr_col_identify_runs(x, size, v_where); break;
  case VCTRS_TYPE_raw: raw_col_identify_runs(x, size, v_where); break;
  case VCTRS_TYPE_list: list_col_identify_runs(x, size, v_where); break;
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened.");
  case VCTRS_TYPE_scalar: r_abort("Can't compare scalars.");
  default: r_abort("Unimplemented type.");
  }
}

#define VEC_COL_IDENTIFY_RUNS(CTYPE, CBEGIN, EQUAL_NA_EQUAL) { \
  CTYPE const* v_x = CBEGIN(x);                                \
  CTYPE ref = v_x[0];                                          \
                                                               \
  for (r_ssize i = 1; i < size; ++i) {                         \
    CTYPE const elt = v_x[i];                                  \
    v_where[i] = v_where[i] && EQUAL_NA_EQUAL(ref, elt);       \
    ref = elt;                                                 \
  }                                                            \
}

static inline
void lgl_col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  VEC_COL_IDENTIFY_RUNS(int, r_lgl_cbegin, lgl_equal_na_equal);
}
static inline
void int_col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  VEC_COL_IDENTIFY_RUNS(int, r_int_cbegin, int_equal_na_equal);
}
static inline
void dbl_col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  VEC_COL_IDENTIFY_RUNS(double, r_dbl_cbegin, dbl_equal_na_equal);
}
static inline
void cpl_col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  VEC_COL_IDENTIFY_RUNS(Rcomplex, r_cpl_cbegin, cpl_equal_na_equal);
}
static inline
void chr_col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  VEC_COL_IDENTIFY_RUNS(r_obj*, r_chr_cbegin, chr_equal_na_equal);
}
static inline
void raw_col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  VEC_COL_IDENTIFY_RUNS(Rbyte, r_raw_cbegin, raw_equal_na_equal);
}
static inline
void list_col_identify_runs(r_obj* x, r_ssize size, bool* v_where) {
  VEC_COL_IDENTIFY_RUNS(r_obj*, r_list_cbegin, list_equal_na_equal);
}

#undef VEC_COL_IDENTIFY_RUNS
