#include "vctrs.h"
#include "utils.h"
#include "equal.h"
#include "translate.h"

static SEXP vec_identify_runs(SEXP x);

// -----------------------------------------------------------------------------

static SEXP vec_locate_runs(SEXP x, bool start);

// [[register()]]
SEXP vctrs_locate_runs(SEXP x, SEXP start) {
  bool c_start = (bool) r_bool_as_int(start);
  return vec_locate_runs(x, c_start);
}

static void vec_locate_run_starts(const int* p_id, r_ssize size, int* p_out);
static void vec_locate_run_ends(const int* p_id, r_ssize size, int* p_out);

static
SEXP vec_locate_runs(SEXP x, bool start) {
  SEXP id = PROTECT(vec_identify_runs(x));
  const int* p_id = INTEGER(id);

  r_ssize size = r_length(id);

  int n = r_int_get(r_attrib_get(id, syms_n), 0);

  SEXP out = PROTECT(r_new_integer(n));
  int* p_out = INTEGER(out);

  if (n == 0) {
    UNPROTECT(2);
    return out;
  }

  if (start) {
    vec_locate_run_starts(p_id, size, p_out);
  } else {
    vec_locate_run_ends(p_id, size, p_out);
  }

  UNPROTECT(2);
  return out;
}

static
void vec_locate_run_starts(const int* p_id, r_ssize size, int* p_out) {
  r_ssize loc = 0;

  // Handle first case
  int ref = p_id[0];
  p_out[loc] = 1;
  ++loc;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[loc] = i + 1;
    ++loc;
  }
}

static
void vec_locate_run_ends(const int* p_id, r_ssize size, int* p_out) {
  r_ssize loc = 0;

  int ref = p_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[loc] = i;
    ++loc;
  }

  // Handle last case
  p_out[loc] = size;
}

// -----------------------------------------------------------------------------

static SEXP vec_detect_runs(SEXP x, bool start);

// [[register()]]
SEXP vctrs_detect_runs(SEXP x, SEXP start) {
  bool c_start = (bool) r_bool_as_int(start);
  return vec_detect_runs(x, c_start);
}

static void vec_detect_run_starts(const int* p_id, r_ssize size, int* p_out);
static void vec_detect_run_ends(const int* p_id, r_ssize size, int* p_out);

static
SEXP vec_detect_runs(SEXP x, bool start) {
  SEXP id = PROTECT(vec_identify_runs(x));
  const int* p_id = INTEGER(id);

  r_ssize size = r_length(id);

  SEXP out = PROTECT(r_new_logical(size));
  int* p_out = LOGICAL(out);
  memset(p_out, 0, size * sizeof(int));

  if (size == 0) {
    UNPROTECT(2);
    return out;
  }

  if (start) {
    vec_detect_run_starts(p_id, size, p_out);
  } else {
    vec_detect_run_ends(p_id, size, p_out);
  }

  UNPROTECT(2);
  return out;
}

static
void vec_detect_run_starts(const int* p_id, r_ssize size, int* p_out) {
  // Handle first case
  int ref = p_id[0];
  p_out[0] = 1;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[i] = 1;
  }
}

static
void vec_detect_run_ends(const int* p_id, r_ssize size, int* p_out) {
  int ref = p_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[i - 1] = 1;
  }

  // Handle last case
  p_out[size - 1] = 1;
}

// -----------------------------------------------------------------------------

// [[register()]]
SEXP vctrs_identify_runs(SEXP x) {
  return vec_identify_runs(x);
}

static int lgl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int int_identify_runs(SEXP x, R_len_t size, int* p_out);
static int dbl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int cpl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int chr_identify_runs(SEXP x, R_len_t size, int* p_out);
static int raw_identify_runs(SEXP x, R_len_t size, int* p_out);
static int list_identify_runs(SEXP x, R_len_t size, int* p_out);
static int df_identify_runs(SEXP x, R_len_t size, int* p_out);

static
SEXP vec_identify_runs(SEXP x) {
  SEXP proxy = PROTECT(vec_proxy_equal(x));
  R_len_t size = vec_size(proxy);
  proxy = PROTECT(proxy_normalize_encoding(proxy));

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  // Handle size 0 up front.
  // All implementations assume at least 1 element.
  if (size == 0) {
    r_attrib_poke(out, syms_n, r_int(0));
    UNPROTECT(3);
    return out;
  }

  enum vctrs_type type = vec_proxy_typeof(proxy);

  int n;

  switch (type) {
  case vctrs_type_logical: n = lgl_identify_runs(proxy, size, p_out); break;
  case vctrs_type_integer: n = int_identify_runs(proxy, size, p_out); break;
  case vctrs_type_double: n = dbl_identify_runs(proxy, size, p_out); break;
  case vctrs_type_complex: n = cpl_identify_runs(proxy, size, p_out); break;
  case vctrs_type_character: n = chr_identify_runs(proxy, size, p_out); break;
  case vctrs_type_raw: n = raw_identify_runs(proxy, size, p_out); break;
  case vctrs_type_list: n = list_identify_runs(proxy, size, p_out); break;
  case vctrs_type_dataframe: n = df_identify_runs(proxy, size, p_out); break;
  default: stop_unimplemented_vctrs_type("vec_identify_runs", type);
  }

  r_attrib_poke(out, syms_n, r_int(n));

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS(CTYPE, CONST_DEREF, SCALAR_EQUAL) {  \
  int id = 1;                                                  \
  const CTYPE* p_x = CONST_DEREF(x);                           \
                                                               \
  /* Handle first case */                                      \
  CTYPE ref = p_x[0];                                          \
  p_out[0] = id;                                               \
                                                               \
  for (R_len_t i = 1; i < size; ++i) {                         \
    const CTYPE elt = p_x[i];                                  \
                                                               \
    if (SCALAR_EQUAL(&elt, &ref) == 0) {                       \
      ++id;                                                    \
      ref = elt;                                               \
    }                                                          \
                                                               \
    p_out[i] = id;                                             \
  }                                                            \
                                                               \
  return id;                                                   \
}

static
int lgl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(int, LOGICAL_RO, lgl_equal_scalar_na_equal);
}
static
int int_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(int, INTEGER_RO, int_equal_scalar_na_equal);
}
static
int dbl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(double, REAL_RO, dbl_equal_scalar_na_equal);
}
static
int cpl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(Rcomplex, COMPLEX_RO, cpl_equal_scalar_na_equal);
}
static
int chr_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(SEXP, STRING_PTR_RO, chr_equal_scalar_na_equal);
}
static
int raw_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(Rbyte, RAW_RO, raw_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS

#define VEC_IDENTIFY_RUNS_BARRIER(SCALAR_EQUAL) {              \
  int id = 1;                                                  \
                                                               \
  /* Handle first case */                                      \
  int loc = 0;                                                 \
  p_out[0] = id;                                               \
                                                               \
  for (R_len_t i = 1; i < size; ++i) {                         \
    if (SCALAR_EQUAL(x, i, x, loc) == 0) {                     \
      ++id;                                                    \
      loc = i;                                                 \
    }                                                          \
                                                               \
    p_out[i] = id;                                             \
  }                                                            \
                                                               \
  return id;                                                   \
}

static
int list_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS_BARRIER(list_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS_BARRIER

// -----------------------------------------------------------------------------

static inline int vec_identify_runs_col(SEXP x,
                                        int id,
                                        struct df_short_circuit_info* p_info,
                                        int* p_out);

static
int df_identify_runs(SEXP x, R_len_t size, int* p_out) {
  int nprot = 0;

  const SEXP* p_x = VECTOR_PTR_RO(x);

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  PROTECT_DF_SHORT_CIRCUIT_INFO(&info, &nprot);

  int id = 1;
  R_len_t n_col = Rf_length(x);

  // Define 0 column case to be a single run
  if (n_col == 0) {
    r_p_int_fill(p_out, id, size);
    UNPROTECT(nprot);
    return id;
  }

  // Handle first case
  p_out[0] = id;
  info.p_row_known[0] = true;
  --info.remaining;

  // Compute non-sequential run IDs
  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = p_x[i];

    id = vec_identify_runs_col(col, id, &info, p_out);

    // All values are unique
    if (info.remaining == 0) {
      break;
    }
  }

  id = 1;
  int previous = p_out[0];

  // Overwrite with sequential IDs
  for (R_len_t i = 1; i < size; ++i) {
    const int current = p_out[i];

    if (current != previous) {
      ++id;
      previous = current;
    }

    p_out[i] = id;
  }

  UNPROTECT(nprot);
  return id;
}

// -----------------------------------------------------------------------------

static int lgl_identify_runs_col(SEXP x,
                                 int id,
                                 struct df_short_circuit_info* p_info,
                                 int* p_out);
static int int_identify_runs_col(SEXP x,
                                 int id,
                                 struct df_short_circuit_info* p_info,
                                 int* p_out);
static int dbl_identify_runs_col(SEXP x,
                                 int id,
                                 struct df_short_circuit_info* p_info,
                                 int* p_out);
static int cpl_identify_runs_col(SEXP x,
                                 int id,
                                 struct df_short_circuit_info* p_info,
                                 int* p_out);
static int chr_identify_runs_col(SEXP x,
                                 int id,
                                 struct df_short_circuit_info* p_info,
                                 int* p_out);
static int raw_identify_runs_col(SEXP x,
                                 int id,
                                 struct df_short_circuit_info* p_info,
                                 int* p_out);
static int list_identify_runs_col(SEXP x,
                                  int id,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);

static inline
int vec_identify_runs_col(SEXP x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* p_out) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: return lgl_identify_runs_col(x, id, p_info, p_out);
  case vctrs_type_integer: return int_identify_runs_col(x, id, p_info, p_out);
  case vctrs_type_double: return dbl_identify_runs_col(x, id, p_info, p_out);
  case vctrs_type_complex: return cpl_identify_runs_col(x, id, p_info, p_out);
  case vctrs_type_character: return chr_identify_runs_col(x, id, p_info, p_out);
  case vctrs_type_raw: return raw_identify_runs_col(x, id, p_info, p_out);
  case vctrs_type_list: return list_identify_runs_col(x, id, p_info, p_out);
  case vctrs_type_dataframe: stop_internal("vec_identify_runs_col", "Data frame columns should be flattened.");
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_identify_runs()`");
  default: Rf_error("Unimplemented type in `vec_identify_runs()`");
  }
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS_COL(CTYPE, CONST_DEREF, EQUAL_SCALAR) { \
  /* First row is always known, so `run_val` and `run_id` */      \
  /* will always be initialized below */                          \
  CTYPE run_val;                                                  \
  int run_id;                                                     \
                                                                  \
  const CTYPE* p_x = CONST_DEREF(x);                              \
                                                                  \
  for (R_len_t i = 0; i < p_info->size; ++i) {                    \
    /* Start of new run */                                        \
    if (p_info->p_row_known[i]) {                                 \
      run_val = p_x[i];                                           \
      run_id = p_out[i];                                          \
      continue;                                                   \
    }                                                             \
                                                                  \
    const CTYPE elt = p_x[i];                                     \
    const int eq = EQUAL_SCALAR(&elt, &run_val);                  \
                                                                  \
    /* Update ID of identical values */                           \
    if (eq != 0) {                                                \
      p_out[i] = run_id;                                          \
      continue;                                                   \
    }                                                             \
                                                                  \
    ++id;                                                         \
    run_val = elt;                                                \
    run_id = id;                                                  \
    p_out[i] = id;                                                \
                                                                  \
    /* This is a run change, so don't check this row again */     \
    p_info->p_row_known[i] = true;                                \
    --p_info->remaining;                                          \
                                                                  \
    if (p_info->remaining == 0) {                                 \
      break;                                                      \
    }                                                             \
  }                                                               \
                                                                  \
  return id;                                                      \
}

static
int lgl_identify_runs_col(SEXP x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* p_out) {
  VEC_IDENTIFY_RUNS_COL(int, LOGICAL_RO, lgl_equal_scalar_na_equal);
}
static
int int_identify_runs_col(SEXP x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* p_out) {
  VEC_IDENTIFY_RUNS_COL(int, INTEGER_RO, int_equal_scalar_na_equal);
}
static
int dbl_identify_runs_col(SEXP x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* p_out) {
  VEC_IDENTIFY_RUNS_COL(double, REAL_RO, dbl_equal_scalar_na_equal);
}
static
int cpl_identify_runs_col(SEXP x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* p_out) {
  VEC_IDENTIFY_RUNS_COL(Rcomplex, COMPLEX_RO, cpl_equal_scalar_na_equal);
}
static
int chr_identify_runs_col(SEXP x,
                          int id,
                          struct df_short_circuit_info* p_info,
                          int* p_out) {
  VEC_IDENTIFY_RUNS_COL(SEXP, STRING_PTR_RO, chr_equal_scalar_na_equal);
}
static
int raw_identify_runs_col(SEXP x,
                          R_len_t id,
                          struct df_short_circuit_info* p_info,
                          int* p_out) {
  VEC_IDENTIFY_RUNS_COL(Rbyte, RAW_RO, raw_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS_COL

#define VEC_IDENTIFY_RUNS_COL_BARRIER(EQUAL_SCALAR) { \
  /* First row is always known, so `run_loc` */       \
  /* and `run_id` will always be initialized below */ \
  R_len_t run_loc;                                    \
  int run_id;                                         \
                                                      \
  for (R_len_t i = 0; i < p_info->size; ++i) {        \
    /* Start of new run */                            \
    if (p_info->p_row_known[i]) {                     \
      run_loc = i;                                    \
      run_id = p_out[i];                              \
      continue;                                       \
    }                                                 \
                                                      \
    const int eq = EQUAL_SCALAR(x, i, x, run_loc);    \
                                                      \
    /* Update ID of identical values */               \
    if (eq != 0) {                                    \
      p_out[i] = run_id;                              \
      continue;                                       \
    }                                                 \
                                                      \
    ++id;                                             \
    run_loc = i;                                      \
    run_id = id;                                      \
    p_out[i] = id;                                    \
                                                      \
    /* This is a run change, */                       \
    /* so don't check this row again */               \
    p_info->p_row_known[i] = true;                    \
    --p_info->remaining;                              \
                                                      \
    if (p_info->remaining == 0) {                     \
      break;                                          \
    }                                                 \
  }                                                   \
                                                      \
  return id;                                          \
}

static
int list_identify_runs_col(SEXP x,
                           int id,
                           struct df_short_circuit_info* p_info,
                           int* p_out) {
  VEC_IDENTIFY_RUNS_COL_BARRIER(list_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS_COL_BARRIER
