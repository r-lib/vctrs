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

  r_obj* out = KEEP(r_new_integer(n));
  int* v_out = r_int_begin(out);

  if (n == 0) {
    FREE(2);
    return out;
  }

  if (start) {
    vec_locate_run_starts(v_id, size, v_out);
  } else {
    vec_locate_run_ends(v_id, size, v_out);
  }

  FREE(2);
  return out;
}

static inline
void vec_locate_run_starts(const int* v_id, r_ssize size, int* v_out) {
  r_ssize loc = 0;

  // Handle first case
  int ref = v_id[0];
  v_out[loc] = 1;
  ++loc;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    v_out[loc] = i + 1;
    ++loc;
  }
}

static inline
void vec_locate_run_ends(const int* v_id, r_ssize size, int* v_out) {
  r_ssize loc = 0;

  int ref = v_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    v_out[loc] = i;
    ++loc;
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
  memset(v_out, 0, size * sizeof(int));

  if (size == 0) {
    FREE(2);
    return out;
  }

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
  // Handle first case
  int ref = v_id[0];
  v_out[0] = 1;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    v_out[i] = 1;
  }
}

static inline
void vec_detect_run_ends(const int* v_id, r_ssize size, int* v_out) {
  int ref = v_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = v_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    v_out[i - 1] = 1;
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
                                                                 \
    if (EQUAL_NA_EQUAL(elt, ref) == 0) {                         \
      ++id;                                                      \
      ref = elt;                                                 \
    }                                                            \
                                                                 \
    v_out[i] = id;                                               \
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
  int n_prot = 0;

  r_obj* const* v_x = r_list_cbegin(x);

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  PROTECT_DF_SHORT_CIRCUIT_INFO(&info, &n_prot);

  int id = 1;
  r_ssize n_col = r_length(x);

  // Define 0 column case to be a single run
  if (n_col == 0) {
    r_p_int_fill(v_out, id, size);
    FREE(n_prot);
    return id;
  }

  // Handle first case
  v_out[0] = id;
  info.p_row_known[0] = true;
  --info.remaining;

  // Compute non-sequential run IDs
  for (r_ssize i = 0; i < n_col; ++i) {
    r_obj* col = v_x[i];

    id = vec_identify_runs_col(col, id, &info, v_out);

    // All values are unique
    if (info.remaining == 0) {
      break;
    }
  }

  id = 1;
  int previous = v_out[0];

  // Overwrite with sequential IDs
  for (r_ssize i = 1; i < size; ++i) {
    const int current = v_out[i];

    if (current != previous) {
      ++id;
      previous = current;
    }

    v_out[i] = id;
  }

  FREE(n_prot);
  return id;
}

// -----------------------------------------------------------------------------

static inline
int vec_identify_runs_col(r_obj* x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* v_out) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: return lgl_identify_runs_col(x, id, p_info, v_out);
  case VCTRS_TYPE_integer: return int_identify_runs_col(x, id, p_info, v_out);
  case VCTRS_TYPE_double: return dbl_identify_runs_col(x, id, p_info, v_out);
  case VCTRS_TYPE_complex: return cpl_identify_runs_col(x, id, p_info, v_out);
  case VCTRS_TYPE_character: return chr_identify_runs_col(x, id, p_info, v_out);
  case VCTRS_TYPE_raw: return raw_identify_runs_col(x, id, p_info, v_out);
  case VCTRS_TYPE_list: return list_identify_runs_col(x, id, p_info, v_out);
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened.");
  case VCTRS_TYPE_scalar: r_abort("Can't compare scalars.");
  default: r_abort("Unimplemented type.");
  }
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS_COL(CTYPE, CBEGIN, EQUAL_NA_EQUAL) {      \
  CTYPE const* v_x = CBEGIN(x);                                     \
                                                                    \
  /* First row is always known, so `run_val` and `run_id` */        \
  /* will always be overwritten immediately below. */               \
  /* But for gcc11 we have to initialize these variables. */        \
  CTYPE run_val = v_x[0];                                           \
  int run_id = 0;                                                   \
                                                                    \
  for (r_ssize i = 0; i < p_info->size; ++i) {                      \
    /* Start of new run */                                          \
    if (p_info->p_row_known[i]) {                                   \
      run_val = v_x[i];                                             \
      run_id = v_out[i];                                            \
      continue;                                                     \
    }                                                               \
                                                                    \
    CTYPE const elt = v_x[i];                                       \
    const int eq = EQUAL_NA_EQUAL(elt, run_val);                    \
                                                                    \
    /* Update ID of identical values */                             \
    if (eq != 0) {                                                  \
      v_out[i] = run_id;                                            \
      continue;                                                     \
    }                                                               \
                                                                    \
    ++id;                                                           \
    run_val = elt;                                                  \
    run_id = id;                                                    \
    v_out[i] = id;                                                  \
                                                                    \
    /* This is a run change, so don't check this row again */       \
    p_info->p_row_known[i] = true;                                  \
    --p_info->remaining;                                            \
                                                                    \
    if (p_info->remaining == 0) {                                   \
      break;                                                        \
    }                                                               \
  }                                                                 \
                                                                    \
  return id;                                                        \
}

static inline
int lgl_identify_runs_col(r_obj* x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* v_out) {
  VEC_IDENTIFY_RUNS_COL(int, r_lgl_cbegin, lgl_equal_na_equal);
}
static inline
int int_identify_runs_col(r_obj* x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* v_out) {
  VEC_IDENTIFY_RUNS_COL(int, r_int_cbegin, int_equal_na_equal);
}
static inline
int dbl_identify_runs_col(r_obj* x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* v_out) {
  VEC_IDENTIFY_RUNS_COL(double, r_dbl_cbegin, dbl_equal_na_equal);
}
static inline
int cpl_identify_runs_col(r_obj* x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* v_out) {
  VEC_IDENTIFY_RUNS_COL(Rcomplex, r_cpl_cbegin, cpl_equal_na_equal);
}
static inline
int chr_identify_runs_col(r_obj* x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* v_out) {
  VEC_IDENTIFY_RUNS_COL(r_obj*, r_chr_cbegin, chr_equal_na_equal);
}
static inline
int raw_identify_runs_col(r_obj* x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* v_out) {
  VEC_IDENTIFY_RUNS_COL(Rbyte, r_raw_cbegin, raw_equal_na_equal);
}
static inline
int list_identify_runs_col(r_obj* x,
                           int id,
                           struct df_short_circuit_info* p_info,
                           int* v_out) {
  VEC_IDENTIFY_RUNS_COL(r_obj*, r_list_cbegin, list_equal_na_equal);
}

#undef VEC_IDENTIFY_RUNS_COL
