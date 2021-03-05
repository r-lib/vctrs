#include "vctrs.h"
#include "equal.h"
#include "type-data-frame.h"

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

// [[ register() ]]
SEXP vctrs_detect_complete(SEXP x) {
  return vec_detect_complete(x);
}

static inline void vec_detect_complete_col(SEXP x, R_len_t size, int* p_out);

static
SEXP vec_detect_complete(SEXP x) {
  if (!is_data_frame(x)) {
    r_abort("`x` must be a data frame.");
  }

  SEXP proxy = PROTECT(vec_proxy_complete(x));

  R_len_t size = vec_size(proxy);

  SEXP out = PROTECT(r_new_logical(size));
  int* p_out = LOGICAL(out);

  // Initialize assuming fully complete
  for (R_len_t i = 0; i < size; ++i) {
    p_out[i] = 1;
  }

  // Proxy unwrapping might have flattened `x`
  if (!is_data_frame(proxy)) {
    vec_detect_complete_col(proxy, size, p_out);
    UNPROTECT(2);
    return out;
  }

  r_ssize n_cols = r_length(proxy);
  const SEXP* p_proxy = VECTOR_PTR_RO(proxy);

  for (r_ssize i = 0; i < n_cols; ++i) {
    vec_detect_complete_col(p_proxy[i], size, p_out);
  }

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

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
  case vctrs_type_dataframe: stop_internal("vec_detect_complete_col", "Data frame columns should have been flattened by now.");
  case vctrs_type_scalar: stop_internal("vec_detect_complete_col", "Can't detect missing values in scalars.");
  default: stop_unimplemented_vctrs_type("vec_detect_complete_col", vec_proxy_typeof(x));
  }
}

// -----------------------------------------------------------------------------

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

#define VEC_DETECT_COMPLETE_COL(CTYPE, CONST_DEREF, IS_MISSING) { \
  const CTYPE* p_x = CONST_DEREF(x);                              \
                                                                  \
  for (R_len_t i = 0; i < size; ++i) {                            \
    const CTYPE elt = p_x[i];                                     \
                                                                  \
    if (IS_MISSING(elt)) {                                        \
      p_out[i] = 0;                                               \
    }                                                             \
  }                                                               \
}

static inline
void lgl_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(int, LOGICAL_RO, lgl_is_missing);
}
static inline
void int_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(int, INTEGER_RO, int_is_missing);
}
static inline
void dbl_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(double, REAL_RO, dbl_is_missing);
}
static inline
void cpl_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(Rcomplex, COMPLEX_RO, cpl_is_missing);
}
static inline
void chr_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(SEXP, STRING_PTR_RO, chr_is_missing);
}
static inline
void raw_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(Rbyte, RAW_RO, raw_is_missing);
}
static inline
void list_detect_complete_col(SEXP x, R_len_t size, int* p_out) {
  VEC_DETECT_COMPLETE_COL(SEXP, VECTOR_PTR_RO, list_is_missing);
}
