#include "vctrs.h"
#include "utils.h"
#include "equal.h"

static SEXP vec_identify_runs(SEXP x);

// [[register()]]
SEXP vctrs_identify_runs(SEXP x) {
  return vec_identify_runs(x);
}

static SEXP lgl_identify_runs(SEXP x, R_len_t size);
static SEXP int_identify_runs(SEXP x, R_len_t size);
static SEXP dbl_identify_runs(SEXP x, R_len_t size);
static SEXP cpl_identify_runs(SEXP x, R_len_t size);
static SEXP chr_identify_runs(SEXP x, R_len_t size);
static SEXP raw_identify_runs(SEXP x, R_len_t size);
static SEXP list_identify_runs(SEXP x, R_len_t size);
static SEXP df_identify_runs(SEXP x, R_len_t size);

static SEXP vec_identify_runs(SEXP x) {
  x = PROTECT(vec_proxy_equal(x));
  R_len_t size = vec_size(x);
  x = PROTECT(obj_maybe_translate_encoding(x, size));

  enum vctrs_type type = vec_proxy_typeof(x);

  SEXP out;

  switch (type) {
  case vctrs_type_logical: out = lgl_identify_runs(x, size); break;
  case vctrs_type_integer: out = int_identify_runs(x, size); break;
  case vctrs_type_double: out = dbl_identify_runs(x, size); break;
  case vctrs_type_complex: out = cpl_identify_runs(x, size); break;
  case vctrs_type_character: out = chr_identify_runs(x, size); break;
  case vctrs_type_raw: out = raw_identify_runs(x, size); break;
  case vctrs_type_list: out = list_identify_runs(x, size); break;
  case vctrs_type_dataframe: out = df_identify_runs(x, size); break;
  default: stop_unimplemented_vctrs_type("vec_identify_runs", type);
  }

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS(CTYPE, CONST_DEREF, SCALAR_EQUAL) {  \
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));            \
  int* p_out = INTEGER(out);                                   \
                                                               \
  if (size == 0) {                                             \
    UNPROTECT(1);                                              \
    return out;                                                \
  }                                                            \
                                                               \
  R_len_t id = 1;                                              \
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
  UNPROTECT(1);                                                \
  return out;                                                  \
}

static SEXP lgl_identify_runs(SEXP x, R_len_t size) {
  VEC_IDENTIFY_RUNS(int, LOGICAL_RO, lgl_equal_scalar_na_equal);
}
static SEXP int_identify_runs(SEXP x, R_len_t size) {
  VEC_IDENTIFY_RUNS(int, INTEGER_RO, int_equal_scalar_na_equal);
}
static SEXP dbl_identify_runs(SEXP x, R_len_t size) {
  VEC_IDENTIFY_RUNS(double, REAL_RO, dbl_equal_scalar_na_equal);
}
static SEXP cpl_identify_runs(SEXP x, R_len_t size) {
  VEC_IDENTIFY_RUNS(Rcomplex, COMPLEX_RO, cpl_equal_scalar_na_equal);
}
static SEXP chr_identify_runs(SEXP x, R_len_t size) {
  VEC_IDENTIFY_RUNS(SEXP, STRING_PTR_RO, chr_equal_scalar_na_equal);
}
static SEXP raw_identify_runs(SEXP x, R_len_t size) {
  VEC_IDENTIFY_RUNS(Rbyte, RAW_RO, raw_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS

#define VEC_IDENTIFY_RUNS_BARRIER(SCALAR_EQUAL) {              \
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));            \
  int* p_out = INTEGER(out);                                   \
                                                               \
  if (size == 0) {                                             \
    UNPROTECT(1);                                              \
    return out;                                                \
  }                                                            \
                                                               \
  R_len_t id = 1;                                              \
                                                               \
  /* Handle first case */                                      \
  int ref = 0;                                                 \
  p_out[0] = id;                                               \
                                                               \
  for (R_len_t i = 1; i < size; ++i) {                         \
    if (SCALAR_EQUAL(x, i, x, ref) == 0) {                     \
      ++id;                                                    \
      ref = i;                                                 \
    }                                                          \
                                                               \
    p_out[i] = id;                                             \
  }                                                            \
                                                               \
  UNPROTECT(1);                                                \
  return out;                                                  \
}                                                              \

static SEXP list_identify_runs(SEXP x, R_len_t size) {
  VEC_IDENTIFY_RUNS_BARRIER(list_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS_BARRIER

// -----------------------------------------------------------------------------

static void df_identify_runs_impl(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);

static SEXP df_identify_runs(SEXP x, R_len_t size) {
  int nprot = 0;

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, size), &nprot);
  int* p_out = INTEGER(out);

  struct df_short_circuit_info info = new_df_short_circuit_info(size);
  struct df_short_circuit_info* p_info = &info;
  PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, &nprot);

  df_identify_runs_impl(x, p_info, p_out);

  UNPROTECT(nprot);
  return out;
}

static inline void vec_identify_runs_col(SEXP x,
                                         struct df_short_circuit_info* p_info,
                                         int* p_out);

static void df_identify_runs_impl(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  // Specially handle size 0 case.
  // Require at least 1 row to extract a "reference".
  if (p_info->size == 0) {
    return;
  }

  // First start value is always true
  p_info->p_row_known[0] = true;
  --p_info->remaining;

  R_len_t n_col = Rf_length(x);

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(x, i);

    vec_identify_runs_col(col, p_info, p_out);

    // All values are unique
    if (p_info->remaining == 0) {
      break;
    }
  }
}

// -----------------------------------------------------------------------------

static void lgl_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);
static void int_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);
static void dbl_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);
static void cpl_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);
static void chr_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);
static void raw_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out);
static void list_identify_runs_col(SEXP x,
                                   struct df_short_circuit_info* p_info,
                                   int* p_out);

static inline void vec_identify_runs_col(SEXP x,
                                         struct df_short_circuit_info* p_info,
                                         int* p_out) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: lgl_identify_runs_col(x, p_info, p_out); break;
  case vctrs_type_integer: int_identify_runs_col(x, p_info, p_out); break;
  case vctrs_type_double: dbl_identify_runs_col(x, p_info, p_out); break;
  case vctrs_type_complex: cpl_identify_runs_col(x, p_info, p_out); break;
  case vctrs_type_character: chr_identify_runs_col(x, p_info, p_out); break;
  case vctrs_type_raw: raw_identify_runs_col(x, p_info, p_out); break;
  case vctrs_type_list: list_identify_runs_col(x, p_info, p_out); break;
  case vctrs_type_dataframe: stop_internal("vec_identify_runs_col", "Data frame columns should be flattened.");
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_identify_runs()`");
  default: Rf_error("Unimplemented type in `vec_identify_runs()`");
  }
}

// -----------------------------------------------------------------------------

static inline void p_int_inc_between(int* p_x, const R_len_t i, const R_len_t j);

#define VEC_IDENTIFY_RUNS_COL(CTYPE, CONST_DEREF, EQUAL_SCALAR) { \
  CTYPE ref;                                                      \
  const CTYPE* p_x = CONST_DEREF(x);                              \
                                                                  \
  for (R_len_t i = 0; i < p_info->size; ++i) {                    \
    /* Start of new run */                                        \
    if (p_info->p_row_known[i]) {                                 \
      ref = p_x[i];                                               \
      continue;                                                   \
    }                                                             \
                                                                  \
    const CTYPE elt = p_x[i];                                     \
    const int eq = EQUAL_SCALAR(&elt, &ref);                      \
                                                                  \
    if (eq == 0) {                                                \
      p_int_inc_between(p_out, i, p_info->size);                  \
      p_info->p_row_known[i] = true;                              \
      --p_info->remaining;                                        \
      ref = elt;                                                  \
                                                                  \
      if (p_info->remaining == 0) {                               \
        break;                                                    \
      }                                                           \
    }                                                             \
  }                                                               \
}

static void lgl_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  VEC_IDENTIFY_RUNS_COL(int, LOGICAL_RO, lgl_equal_scalar_na_equal);
}
static void int_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  VEC_IDENTIFY_RUNS_COL(int, INTEGER_RO, int_equal_scalar_na_equal);
}
static void dbl_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  VEC_IDENTIFY_RUNS_COL(double, REAL_RO, dbl_equal_scalar_na_equal);
}
static void cpl_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  VEC_IDENTIFY_RUNS_COL(Rcomplex, COMPLEX_RO, cpl_equal_scalar_na_equal);
}
static void chr_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  VEC_IDENTIFY_RUNS_COL(SEXP, STRING_PTR_RO, chr_equal_scalar_na_equal);
}
static void raw_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  VEC_IDENTIFY_RUNS_COL(Rbyte, RAW_RO, raw_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS_COL

#define VEC_IDENTIFY_RUNS_COL_BARRIER(EQUAL_SCALAR) { \
  R_len_t ref = 0;                                    \
                                                      \
  for (R_len_t i = 0; i < p_info->size; ++i) {        \
    /* Start of new run */                            \
    if (p_info->p_row_known[i]) {                     \
      ref = i;                                        \
      continue;                                       \
    }                                                 \
                                                      \
    const int eq = EQUAL_SCALAR(x, i, x, ref);        \
                                                      \
    if (eq == 0) {                                    \
      p_int_inc_between(p_out, i, p_info->size);      \
      p_info->p_row_known[i] = true;                  \
      --p_info->remaining;                            \
      ref = i;                                        \
                                                      \
      if (p_info->remaining == 0) {                   \
        break;                                        \
      }                                               \
    }                                                 \
  }                                                   \
}

static void list_identify_runs_col(SEXP x,
                                  struct df_short_circuit_info* p_info,
                                  int* p_out) {
  VEC_IDENTIFY_RUNS_COL_BARRIER(list_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS_COL_BARRIER

// -----------------------------------------------------------------------------

static inline void p_int_inc_between(int* p_x, const R_len_t i, const R_len_t j) {
  for (R_len_t k = i; k < j; ++k) {
    ++p_x[k];
  }
}
