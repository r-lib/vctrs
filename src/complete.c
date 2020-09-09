#include "vctrs.h"
#include "equal.h"

// -----------------------------------------------------------------------------

struct complete_info {
  SEXP data;
  int* p_data;
  r_ssize n;
};

static inline
struct complete_info new_complete_info(r_ssize size) {
  SEXP data = PROTECT(Rf_allocVector(LGLSXP, size));
  int* p_data = LOGICAL(data);

  struct complete_info info = {
    .data = data,
    .p_data = p_data,
    .n = 0
  };

  UNPROTECT(1);
  return info;
}

#define PROTECT_COMPLETE_INFO(p_info, p_n) { \
  PROTECT((p_info)->data);                   \
  *(p_n) += 1;                               \
}

// -----------------------------------------------------------------------------

static SEXP vec_slice_complete(SEXP x);

// [[ register() ]]
SEXP vctrs_slice_complete(SEXP x) {
  return vec_slice_complete(x);
}

static SEXP vec_locate_complete(SEXP x);

static
SEXP vec_slice_complete(SEXP x) {
  SEXP loc = PROTECT(vec_locate_complete(x));

  // Skip `vec_as_location()` in `vec_slice()`
  SEXP out = vec_slice_impl(x, loc);

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_locate_complete(SEXP x) {
  return vec_locate_complete(x);
}

static void proxy_detect_complete(SEXP proxy,
                                  R_len_t size,
                                  struct complete_info* p_info);

static
SEXP vec_locate_complete(SEXP x) {
  int nprot = 0;

  SEXP proxy = PROTECT_N(vec_proxy_equal(x), &nprot);

  R_len_t size = vec_size(proxy);

  struct complete_info info = new_complete_info(size);
  PROTECT_COMPLETE_INFO(&info, &nprot);

  proxy_detect_complete(proxy, size, &info);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, info.n), &nprot);
  int* p_out = INTEGER(out);

  R_len_t loc = 0;

  const int* p_complete = info.p_data;

  for (R_len_t i = 0; i < size; ++i) {
    if (p_complete[i]) {
      p_out[loc] = i + 1;
      ++loc;
    }
  }

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP vec_detect_complete(SEXP x);

// [[ register() ]]
SEXP vctrs_detect_complete(SEXP x) {
  return vec_detect_complete(x);
}

static
SEXP vec_detect_complete(SEXP x) {
  int nprot = 0;

  SEXP proxy = PROTECT_N(vec_proxy_equal(x), &nprot);

  R_len_t size = vec_size(proxy);

  struct complete_info info = new_complete_info(size);
  PROTECT_COMPLETE_INFO(&info, &nprot);

  proxy_detect_complete(proxy, size, &info);

  UNPROTECT(nprot);
  return info.data;
}

// -----------------------------------------------------------------------------

static inline void lgl_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);
static inline void int_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);
static inline void dbl_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);
static inline void cpl_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);
static inline void chr_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);
static inline void raw_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);
static inline void list_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);
static void df_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info);

static inline
void proxy_detect_complete(SEXP proxy,
                           R_len_t size,
                           struct complete_info* p_info) {
  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical: return lgl_detect_complete(proxy, size, p_info);
  case vctrs_type_integer: return int_detect_complete(proxy, size, p_info);
  case vctrs_type_double: return dbl_detect_complete(proxy, size, p_info);
  case vctrs_type_complex: return cpl_detect_complete(proxy, size, p_info);
  case vctrs_type_character: return chr_detect_complete(proxy, size, p_info);
  case vctrs_type_raw: return raw_detect_complete(proxy, size, p_info);
  case vctrs_type_list: return list_detect_complete(proxy, size, p_info);
  case vctrs_type_dataframe: return df_detect_complete(proxy, size, p_info);
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't detect missing values in scalars with `vec_detect_complete()`.");
  default: Rf_errorcall(R_NilValue, "Unimplemented type in `vec_detect_complete()`.");
  }
}

#define VEC_DETECT_COMPLETE(CTYPE, CONST_DEREF, SCALAR_EQUAL_MISSING) { \
  const CTYPE* p_x = CONST_DEREF(x);                                    \
                                                                        \
  for (R_len_t i = 0; i < size; ++i) {                                  \
    const CTYPE elt = p_x[i];                                           \
    const bool complete = !SCALAR_EQUAL_MISSING(elt);                   \
    p_info->p_data[i] = complete;                                       \
    p_info->n += complete;                                              \
  }                                                                     \
}

static inline
void lgl_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  VEC_DETECT_COMPLETE(int, LOGICAL_RO, lgl_scalar_equal_missing);
}
static inline
void int_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  VEC_DETECT_COMPLETE(int, INTEGER_RO, int_scalar_equal_missing);
}
static inline
void dbl_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  VEC_DETECT_COMPLETE(double, REAL_RO, dbl_scalar_equal_missing);
}
static inline
void cpl_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  VEC_DETECT_COMPLETE(Rcomplex, COMPLEX_RO, cpl_scalar_equal_missing);
}
static inline
void chr_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  VEC_DETECT_COMPLETE(SEXP, STRING_PTR_RO, chr_scalar_equal_missing);
}
static inline
void raw_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  VEC_DETECT_COMPLETE(Rbyte, RAW_RO, raw_scalar_equal_missing);
}

#undef VEC_DETECT_COMPLETE

#define VEC_DETECT_COMPLETE_BARRIER(SCALAR_EQUAL_MISSING) {    \
  for (R_len_t i = 0; i < size; ++i) {                         \
    const bool complete = !SCALAR_EQUAL_MISSING(x, i);         \
    p_info->p_data[i] = complete;                              \
    p_info->n += complete;                                     \
  }                                                            \
}

static inline
void list_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  VEC_DETECT_COMPLETE_BARRIER(list_scalar_equal_missing);
}

#undef VEC_DETECT_COMPLETE_BARRIER

// -----------------------------------------------------------------------------

static inline void vec_detect_complete_col(SEXP x,
                                           R_len_t size,
                                           int* p_out,
                                           struct df_short_circuit_info* p_df_info);

static
void df_detect_complete(SEXP x, R_len_t size, struct complete_info* p_info) {
  int nprot = 0;

  int* p_out = p_info->p_data;

  // Initialize assuming complete
  for (R_len_t i = 0; i < size; ++i) {
    p_out[i] = 1;
  }

  struct df_short_circuit_info df_info = new_df_short_circuit_info(size);
  PROTECT_DF_SHORT_CIRCUIT_INFO(&df_info, &nprot);

  int n_col = Rf_length(x);

  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (R_len_t i = 0; i < n_col; ++i) {
    const SEXP col = p_x[i];

    vec_detect_complete_col(col, size, p_out, &df_info);

    // If we know every row is not complete, break
    if (df_info.remaining == 0) {
      break;
    }
  }

  p_info->n = df_info.remaining;

  UNPROTECT(nprot);
}

static void lgl_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info);
static void int_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info);
static void dbl_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info);
static void cpl_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info);
static void chr_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info);
static void raw_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info);
static void list_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info);

static inline
void vec_detect_complete_col(SEXP x,
                             R_len_t size,
                             int* p_out,
                             struct df_short_circuit_info* p_df_info) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: return lgl_detect_complete_col(x, size, p_out, p_df_info);
  case vctrs_type_integer: return int_detect_complete_col(x, size, p_out, p_df_info);
  case vctrs_type_double: return dbl_detect_complete_col(x, size, p_out, p_df_info);
  case vctrs_type_complex: return cpl_detect_complete_col(x, size, p_out, p_df_info);
  case vctrs_type_character: return chr_detect_complete_col(x, size, p_out, p_df_info);
  case vctrs_type_raw: return raw_detect_complete_col(x, size, p_out, p_df_info);
  case vctrs_type_list: return list_detect_complete_col(x, size, p_out, p_df_info);
  case vctrs_type_dataframe: stop_internal("vec_detect_complete_col", "Data frame columns should have been handled already.");
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't detect missing values in scalars with `vec_detect_complete()`.");
  default: Rf_errorcall(R_NilValue, "Unimplemented type in `vec_detect_complete()`.");
  }
}

#define VEC_DETECT_COMPLETE_COL(CTYPE, CONST_DEREF, SCALAR_EQUAL_MISSING) { \
  const CTYPE* p_x = CONST_DEREF(x);                                        \
                                                                            \
  for (R_len_t i = 0; i < size; ++i) {                                      \
    /* Row already has missing values */                                    \
    if (p_df_info->p_row_known[i]) {                                        \
      continue;                                                             \
    }                                                                       \
                                                                            \
    const CTYPE elt = p_x[i];                                               \
    const bool complete = !SCALAR_EQUAL_MISSING(elt);                       \
                                                                            \
    /* Row is still fully complete */                                       \
    if (complete) {                                                         \
      continue;                                                             \
    }                                                                       \
                                                                            \
    p_out[i] = 0;                                                           \
                                                                            \
    /* Mark row as incomplete */                                            \
    p_df_info->p_row_known[i] = true;                                       \
    --p_df_info->remaining;                                                 \
                                                                            \
    if (p_df_info->remaining == 0) {                                        \
      break;                                                                \
    }                                                                       \
  }                                                                         \
}

static
void lgl_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info) {
  VEC_DETECT_COMPLETE_COL(int, LOGICAL_RO, lgl_scalar_equal_missing);
}
static
void int_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info) {
  VEC_DETECT_COMPLETE_COL(int, INTEGER_RO, int_scalar_equal_missing);
}
static
void dbl_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info) {
  VEC_DETECT_COMPLETE_COL(double, REAL_RO, dbl_scalar_equal_missing);
}
static
void cpl_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info) {
  VEC_DETECT_COMPLETE_COL(Rcomplex, COMPLEX_RO, cpl_scalar_equal_missing);
}
static
void chr_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info) {
  VEC_DETECT_COMPLETE_COL(SEXP, STRING_PTR_RO, chr_scalar_equal_missing);
}
static
void raw_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info) {
  VEC_DETECT_COMPLETE_COL(Rbyte, RAW_RO, raw_scalar_equal_missing);
}

#undef VEC_DETECT_COMPLETE_COL

#define VEC_DETECT_COMPLETE_COL_BARRIER(SCALAR_EQUAL_MISSING) { \
  for (R_len_t i = 0; i < size; ++i) {                          \
    /* Row already has missing values */                        \
    if (p_df_info->p_row_known[i]) {                            \
      continue;                                                 \
    }                                                           \
                                                                \
    const bool complete = !SCALAR_EQUAL_MISSING(x, i);          \
                                                                \
    /* Row is still fully complete */                           \
    if (complete) {                                             \
      continue;                                                 \
    }                                                           \
                                                                \
    p_out[i] = 0;                                               \
                                                                \
    /* Mark row as incomplete */                                \
    p_df_info->p_row_known[i] = true;                           \
    --p_df_info->remaining;                                     \
                                                                \
    if (p_df_info->remaining == 0) {                            \
      break;                                                    \
    }                                                           \
  }                                                             \
}

static
void list_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_df_info) {
  VEC_DETECT_COMPLETE_COL_BARRIER(list_scalar_equal_missing);
}

#undef VEC_DETECT_COMPLETE_COL_BARRIER
