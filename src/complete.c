#include "vctrs.h"
#include "equal.h"

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

static SEXP vec_detect_complete(SEXP x);

static
SEXP vec_locate_complete(SEXP x) {
  SEXP where = PROTECT(vec_detect_complete(x));
  SEXP out = r_lgl_which(where, false);
  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static inline void lgl_detect_complete(SEXP x, R_len_t size, int* p_out);
static inline void int_detect_complete(SEXP x, R_len_t size, int* p_out);
static inline void dbl_detect_complete(SEXP x, R_len_t size, int* p_out);
static inline void cpl_detect_complete(SEXP x, R_len_t size, int* p_out);
static inline void chr_detect_complete(SEXP x, R_len_t size, int* p_out);
static inline void raw_detect_complete(SEXP x, R_len_t size, int* p_out);
static inline void list_detect_complete(SEXP x, R_len_t size, int* p_out);
static void df_detect_complete(SEXP x, R_len_t size, int* p_out);

// [[ register() ]]
SEXP vctrs_detect_complete(SEXP x) {
  return vec_detect_complete(x);
}

static
SEXP vec_detect_complete(SEXP x) {
  SEXP proxy = PROTECT(vec_proxy_equal(x));
  R_len_t size = vec_size(proxy);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));
  int* p_out = LOGICAL(out);

  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical: lgl_detect_complete(proxy, size, p_out); break;
  case vctrs_type_integer: int_detect_complete(proxy, size, p_out); break;
  case vctrs_type_double: dbl_detect_complete(proxy, size, p_out); break;
  case vctrs_type_complex: cpl_detect_complete(proxy, size, p_out); break;
  case vctrs_type_character: chr_detect_complete(proxy, size, p_out); break;
  case vctrs_type_raw: raw_detect_complete(proxy, size, p_out); break;
  case vctrs_type_list: list_detect_complete(proxy, size, p_out); break;
  case vctrs_type_dataframe: df_detect_complete(proxy, size, p_out); break;
  case vctrs_type_scalar: stop_internal("vec_detect_complete", "Can't detect missing values in scalars.");
  default: stop_unimplemented_vctrs_type("vec_detect_complete", vec_proxy_typeof(proxy));
  }

  UNPROTECT(2);
  return out;
}

#define VEC_DETECT_COMPLETE(CTYPE, CONST_DEREF, SCALAR_EQUAL_MISSING) { \
  const CTYPE* p_x = CONST_DEREF(x);                                    \
                                                                        \
  for (R_len_t i = 0; i < size; ++i) {                                  \
    const CTYPE elt = p_x[i];                                           \
    p_out[i] = !SCALAR_EQUAL_MISSING(elt);                              \
  }                                                                     \
}

static inline
void lgl_detect_complete(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE(int, LOGICAL_RO, lgl_equal_missing_scalar);
}
static inline
void int_detect_complete(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE(int, INTEGER_RO, int_equal_missing_scalar);
}
static inline
void dbl_detect_complete(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE(double, REAL_RO, dbl_equal_missing_scalar);
}
static inline
void cpl_detect_complete(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE(Rcomplex, COMPLEX_RO, cpl_equal_missing_scalar);
}
static inline
void chr_detect_complete(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE(SEXP, STRING_PTR_RO, chr_equal_missing_scalar);
}
static inline
void raw_detect_complete(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE(Rbyte, RAW_RO, raw_equal_missing_scalar);
}

#undef VEC_DETECT_COMPLETE

#define VEC_DETECT_COMPLETE_BARRIER(SCALAR_EQUAL_MISSING) { \
  for (R_len_t i = 0; i < size; ++i) {                      \
    p_out[i] = !SCALAR_EQUAL_MISSING(x, i);                 \
  }                                                         \
}

static inline
void list_detect_complete(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_BARRIER(list_equal_missing_scalar);
}

#undef VEC_DETECT_COMPLETE_BARRIER

// -----------------------------------------------------------------------------

static inline void vec_detect_complete_col(SEXP x, R_len_t size, int* p_out);

static
void df_detect_complete(SEXP x, R_len_t size, int* p_out) {
  // Initialize assuming fully complete
  for (R_len_t i = 0; i < size; ++i) {
    p_out[i] = 1;
  }

  int n_col = Rf_length(x);
  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (R_len_t i = 0; i < n_col; ++i) {
    vec_detect_complete_col(p_x[i], size, p_out);
  }
}

static inline void lgl_detect_complete_col(SEXP x, R_len_t size, int* p_out);
static inline void int_detect_complete_col(SEXP x, R_len_t size, int* p_out);
static inline void dbl_detect_complete_col(SEXP x, R_len_t size, int* p_out);
static inline void cpl_detect_complete_col(SEXP x, R_len_t size, int* p_out);
static inline void chr_detect_complete_col(SEXP x, R_len_t size, int* p_out);
static inline void raw_detect_complete_col(SEXP x, R_len_t size, int* p_out);
static inline void list_detect_complete_col(SEXP x, R_len_t size, int* p_out);

static inline
void vec_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: lgl_detect_complete_col(x, size, p_out); break;
  case vctrs_type_integer: int_detect_complete_col(x, size, p_out); break;
  case vctrs_type_double: dbl_detect_complete_col(x, size, p_out); break;
  case vctrs_type_complex: cpl_detect_complete_col(x, size, p_out); break;
  case vctrs_type_character: chr_detect_complete_col(x, size, p_out); break;
  case vctrs_type_raw: raw_detect_complete_col(x, size, p_out); break;
  case vctrs_type_list: list_detect_complete_col(x, size, p_out); break;
  case vctrs_type_dataframe: stop_internal("vec_detect_complete_col", "Data frame columns should have been handled already.");
  case vctrs_type_scalar: stop_internal("vec_detect_complete_col", "Can't detect missing values in scalars.");
  default: stop_unimplemented_vctrs_type("vec_detect_complete_col", vec_proxy_typeof(x));
  }
}


/*
 * Avoid the temptation to add an extra if branch at the start of the for
 * loop like:
 *
 * ```
 * if (!p_out[i]) {
 *   continue;
 * }
 * ```
 *
 * In theory this avoids calculations if we already know the row is incomplete,
 * but in practice it can wreck performance. I imagine it is due to the cost
 * of the extra branch + the volatility of this value, causing the result of
 * the branch to be "guessed" incorrectly many times. For example, the vctrs
 * result here gets 6x slower (i.e. slower than the R solution) by adding that
 * branch.
 *
 * ```
 * # Place many NA values randomly in the first column
 * first <- sample(c(1, NA, 3), size = 1e6, replace = TRUE)
 * cols <- rep_len(list(rep(1, 1e6)), 100)
 * cols <- c(list(first), cols)
 * names(cols) <- paste0("a", 1:length(cols))
 * df <- new_data_frame(cols)
 * bench::mark(vec_detect_complete(df), complete.cases(df))
 * ```
 */

#define VEC_DETECT_COMPLETE_COL(CTYPE, CONST_DEREF, SCALAR_EQUAL_MISSING) { \
  const CTYPE* p_x = CONST_DEREF(x);                                        \
                                                                            \
  for (R_len_t i = 0; i < size; ++i) {                                      \
    const CTYPE elt = p_x[i];                                               \
                                                                            \
    if (SCALAR_EQUAL_MISSING(elt)) {                                        \
      p_out[i] = 0;                                                         \
    }                                                                       \
  }                                                                         \
}

static inline
void lgl_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(int, LOGICAL_RO, lgl_equal_missing_scalar);
}
static inline
void int_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(int, INTEGER_RO, int_equal_missing_scalar);
}
static inline
void dbl_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(double, REAL_RO, dbl_equal_missing_scalar);
}
static inline
void cpl_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(Rcomplex, COMPLEX_RO, cpl_equal_missing_scalar);
}
static inline
void chr_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(SEXP, STRING_PTR_RO, chr_equal_missing_scalar);
}
static inline
void raw_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(Rbyte, RAW_RO, raw_equal_missing_scalar);
}

#undef VEC_DETECT_COMPLETE_COL

#define VEC_DETECT_COMPLETE_COL_BARRIER(SCALAR_EQUAL_MISSING) { \
  for (R_len_t i = 0; i < size; ++i) {                          \
    if (SCALAR_EQUAL_MISSING(x, i)) {                           \
      p_out[i] = 0;                                             \
    }                                                           \
  }                                                             \
}

static inline
void list_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL_BARRIER(list_equal_missing_scalar);
}

#undef VEC_DETECT_COMPLETE_COL_BARRIER
