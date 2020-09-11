#include "vctrs.h"
#include "equal.h"
#include "type-data-frame.h"

// -----------------------------------------------------------------------------

static SEXP df_slice_complete(SEXP x);

// [[ register() ]]
SEXP vctrs_df_slice_complete(SEXP x) {
  return df_slice_complete(x);
}

static SEXP df_locate_complete(SEXP x);

static
SEXP df_slice_complete(SEXP x) {
  SEXP loc = PROTECT(df_locate_complete(x));

  // Skip `vec_as_location()` in `vec_slice()`
  SEXP out = vec_slice_impl(x, loc);

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_df_locate_complete(SEXP x) {
  return df_locate_complete(x);
}

static SEXP df_detect_complete(SEXP x);

static
SEXP df_locate_complete(SEXP x) {
  SEXP where = PROTECT(df_detect_complete(x));
  SEXP out = r_lgl_which(where, false);
  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_df_detect_complete(SEXP x) {
  return df_detect_complete(x);
}

static inline void vec_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_info);

static
SEXP df_detect_complete(SEXP x) {
  int nprot = 0;

  if (!is_data_frame(x)) {
    r_abort("`x` must be a data frame.");
  }

  // Flatten df-cols, but don't proxy
  x = PROTECT_N(df_flatten(x), &nprot);

  R_len_t size = vec_size(x);
  r_ssize n_cols = r_length(x);

  SEXP out = PROTECT_N(r_new_logical(size), &nprot);
  int* p_out = LOGICAL(out);

  // Initialize assuming fully complete
  for (R_len_t i = 0; i < size; ++i) {
    p_out[i] = 1;
  }

  struct df_short_circuit_info info = new_df_short_circuit_info(size, true);
  PROTECT_DF_SHORT_CIRCUIT_INFO(&info, &nprot);

  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = 0; i < n_cols; ++i) {
    vec_detect_complete_col(p_x[i], size, p_out, &info);
  }

  UNPROTECT(nprot);
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
static void df_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_info);

static inline
void vec_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_info) {
  SEXP proxy = PROTECT(vec_proxy_equal(x));

  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical: lgl_detect_complete_col(proxy, size, p_out); break;
  case vctrs_type_integer: int_detect_complete_col(proxy, size, p_out); break;
  case vctrs_type_double: dbl_detect_complete_col(proxy, size, p_out); break;
  case vctrs_type_complex: cpl_detect_complete_col(proxy, size, p_out); break;
  case vctrs_type_character: chr_detect_complete_col(proxy, size, p_out); break;
  case vctrs_type_raw: raw_detect_complete_col(proxy, size, p_out); break;
  case vctrs_type_list: list_detect_complete_col(proxy, size, p_out); break;
  case vctrs_type_dataframe: df_detect_complete_col(proxy, size, p_out, p_info); break;
  case vctrs_type_scalar: stop_internal("vec_detect_complete_col", "Can't detect missing values in scalars.");
  default: stop_unimplemented_vctrs_type("vec_detect_complete_col", vec_proxy_typeof(proxy));
  }

  UNPROTECT(1);
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
 * bench::mark(df_detect_complete(df), complete.cases(df))
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

// -----------------------------------------------------------------------------

static void vec_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);

/*
 * This data frame path occurs when a vector that isn't a data frame has a
 * data frame proxy, like a rcrd. In these cases, we consider a "complete"
 * row to be any row that has at least one non-missing value.
 */

static
void df_detect_complete_col(SEXP x, R_len_t size, int* p_out, struct df_short_circuit_info* p_info) {
  r_ssize n_cols = r_length(x);
  const SEXP* p_x = VECTOR_PTR_RO(x);

  // Ensure lazy memory is initialized
  init_lazy_df_short_circuit_info(p_info);

  // Reset between each df-col
  p_info->remaining = size;

  // If `p_out[i] == true`, we don't yet know the row value since
  // `true` was the default value. Otherwise we already know it to be an
  // incomplete row and can skip.
  for (r_ssize i = 0; i < size; ++i) {
    const bool known = !p_out[i];
    p_info->p_row_known[i] = known;
    p_info->remaining -= known;
  }

  for (r_ssize i = 0; i < n_cols; ++i) {
    SEXP col = p_x[i];

    vec_detect_any_non_missing_col(col, size, p_info);

    if (p_info->remaining == 0) {
      break;
    }
  }

  // If we didn't know the row value before (`p_out[i] == true`),
  // and we still don't (`p_row_known[i] == false`),
  // then the entire row of this df-col is missing,
  // which we consider incomplete.
  for (r_ssize i = 0; i < size; ++i) {
    if (p_out[i] && !p_info->p_row_known[i]) {
      p_out[i] = 0;
    }
  }
}

static inline void lgl_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);
static inline void int_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);
static inline void dbl_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);
static inline void cpl_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);
static inline void chr_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);
static inline void raw_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);
static inline void list_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info);

static
void vec_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: lgl_detect_any_non_missing_col(x, size, p_info); break;
  case vctrs_type_integer: int_detect_any_non_missing_col(x, size, p_info); break;
  case vctrs_type_double: dbl_detect_any_non_missing_col(x, size, p_info); break;
  case vctrs_type_complex: cpl_detect_any_non_missing_col(x, size, p_info); break;
  case vctrs_type_character: chr_detect_any_non_missing_col(x, size, p_info); break;
  case vctrs_type_raw: raw_detect_any_non_missing_col(x, size, p_info); break;
  case vctrs_type_list: list_detect_any_non_missing_col(x, size, p_info); break;
  case vctrs_type_dataframe: stop_internal("vec_detect_any_non_missing_col", "Data frame columns should have been handled already.");
  case vctrs_type_scalar: stop_internal("vec_detect_any_non_missing_col", "Can't detect missing values in scalars.");
  default: stop_unimplemented_vctrs_type("vec_detect_any_non_missing_col", vec_proxy_typeof(x));
  }
}

#define VEC_DETECT_ANY_NON_MISSING(CTYPE, CONST_DEREF, SCALAR_EQUAL_MISSING) { \
  const CTYPE* p_x = CONST_DEREF(x);                                           \
                                                                               \
  for (R_len_t i = 0; i < size; ++i) {                                         \
    /* Known to be incomplete row */                                           \
    if (p_info->p_row_known[i]) {                                              \
      continue;                                                                \
    }                                                                          \
                                                                               \
    const CTYPE elt = p_x[i];                                                  \
                                                                               \
    if (SCALAR_EQUAL_MISSING(elt)) {                                           \
      continue;                                                                \
    }                                                                          \
                                                                               \
    /* At least one non-missing value exists */                                \
    p_info->p_row_known[i] = true;                                             \
    --p_info->remaining;                                                       \
                                                                               \
    /* All rows have at least one non-missing value */                         \
    if (p_info->remaining == 0) {                                              \
      break;                                                                   \
    }                                                                          \
  }                                                                            \
}

static inline
void lgl_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  VEC_DETECT_ANY_NON_MISSING(int, LOGICAL_RO, lgl_equal_missing_scalar);
}
static inline
void int_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  VEC_DETECT_ANY_NON_MISSING(int, INTEGER_RO, int_equal_missing_scalar);
}
static inline
void dbl_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  VEC_DETECT_ANY_NON_MISSING(double, REAL_RO, dbl_equal_missing_scalar);
}
static inline
void cpl_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  VEC_DETECT_ANY_NON_MISSING(Rcomplex, COMPLEX_RO, cpl_equal_missing_scalar);
}
static inline
void chr_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  VEC_DETECT_ANY_NON_MISSING(SEXP, STRING_PTR_RO, chr_equal_missing_scalar);
}
static inline
void raw_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  VEC_DETECT_ANY_NON_MISSING(Rbyte, RAW_RO, raw_equal_missing_scalar);
}

#undef VEC_DETECT_ANY_NON_MISSING


#define VEC_DETECT_ANY_NON_MISSING_BARRIER(SCALAR_EQUAL_MISSING) { \
  for (R_len_t i = 0; i < size; ++i) {                             \
    /* Known to be incomplete row */                               \
    if (p_info->p_row_known[i]) {                                  \
      continue;                                                    \
    }                                                              \
                                                                   \
    if (SCALAR_EQUAL_MISSING(x, i)) {                              \
      continue;                                                    \
    }                                                              \
                                                                   \
    /* At least one non-missing value exists */                    \
    p_info->p_row_known[i] = true;                                 \
    --p_info->remaining;                                           \
                                                                   \
    /* All rows have at least one non-missing value */             \
    if (p_info->remaining == 0) {                                  \
      break;                                                       \
    }                                                              \
  }                                                                \
}

static inline
void list_detect_any_non_missing_col(SEXP x, R_len_t size, struct df_short_circuit_info* p_info) {
  VEC_DETECT_ANY_NON_MISSING_BARRIER(list_equal_missing_scalar);
}

#undef VEC_DETECT_ANY_NON_MISSING_BARRIER
