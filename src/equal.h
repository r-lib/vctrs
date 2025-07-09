#ifndef VCTRS_EQUAL_H
#define VCTRS_EQUAL_H

#include "vctrs-core.h"
#include "missing.h"
#include "poly-op.h"

// equal_object() never propagates missingness, so
// it can return a `bool`
bool equal_object(SEXP x, SEXP y);
bool equal_object_normalized(SEXP x, SEXP y);
bool equal_names(SEXP x, SEXP y);

// -----------------------------------------------------------------------------

static inline int lgl_equal_na_equal(int x, int y) {
  return x == y;
}
static inline int int_equal_na_equal(int x, int y) {
  return x == y;
}
static inline int dbl_equal_na_equal(double x, double y) {
  switch (dbl_classify(x)) {
  case VCTRS_DBL_number: break;
  case VCTRS_DBL_missing: return dbl_classify(y) == VCTRS_DBL_missing;
  case VCTRS_DBL_nan: return dbl_classify(y) == VCTRS_DBL_nan;
  }

  return isnan(y) ? false : x == y;
}
static inline int cpl_equal_na_equal(Rcomplex x, Rcomplex y) {
  return dbl_equal_na_equal(x.r, y.r) && dbl_equal_na_equal(x.i, y.i);
}
static inline int chr_equal_na_equal(SEXP x, SEXP y) {
  return x == y;
}
static inline int raw_equal_na_equal(Rbyte x, Rbyte y) {
  return x == y;
}
static inline int list_equal_na_equal(SEXP x, SEXP y) {
  return equal_object_normalized(x, y);
}

// -----------------------------------------------------------------------------

#define P_EQUAL_NA_EQUAL(CTYPE, EQUAL_NA_EQUAL) do {                       \
  return EQUAL_NA_EQUAL(((const CTYPE*) p_x)[i], ((const CTYPE*) p_y)[j]); \
} while (0)

static r_no_return
inline int p_nil_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  r_stop_internal("Can't compare NULL for equality.");
}
static inline int p_lgl_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_EQUAL(int, lgl_equal_na_equal);
}
static inline int p_int_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_EQUAL(int, int_equal_na_equal);
}
static inline int p_dbl_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_EQUAL(double, dbl_equal_na_equal);
}
static inline int p_cpl_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_EQUAL(Rcomplex, cpl_equal_na_equal);
}
static inline int p_chr_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_EQUAL(SEXP, chr_equal_na_equal);
}
static inline int p_raw_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_EQUAL(Rbyte, raw_equal_na_equal);
}
static inline int p_list_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_EQUAL(SEXP, list_equal_na_equal);
}

#undef P_EQUAL_NA_EQUAL

// No support for df-cols, as they should be flattened
static inline
bool p_col_equal_na_equal(
  const void* p_x,
  r_ssize i,
  const void* p_y,
  r_ssize j,
  const enum vctrs_type type
) {
  switch (type) {
  case VCTRS_TYPE_logical: return p_lgl_equal_na_equal(p_x, i, p_y, j);
  case VCTRS_TYPE_integer: return p_int_equal_na_equal(p_x, i, p_y, j);
  case VCTRS_TYPE_double: return p_dbl_equal_na_equal(p_x, i, p_y, j);
  case VCTRS_TYPE_complex: return p_cpl_equal_na_equal(p_x, i, p_y, j);
  case VCTRS_TYPE_character: return p_chr_equal_na_equal(p_x, i, p_y, j);
  case VCTRS_TYPE_raw: return p_raw_equal_na_equal(p_x, i, p_y, j);
  case VCTRS_TYPE_list: return p_list_equal_na_equal(p_x, i, p_y, j);
  default: stop_unimplemented_vctrs_type("p_col_equal_na_equal", type);
  }
}

static inline
int p_df_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  struct poly_df_data* p_x_data = (struct poly_df_data*) p_x;
  struct poly_df_data* p_y_data = (struct poly_df_data*) p_y;

  r_ssize n_col = p_x_data->n_col;
  if (n_col != p_y_data->n_col) {
    r_stop_internal("`x` and `y` must have the same number of columns.");
  }

  enum vctrs_type* v_col_type = p_x_data->v_col_type;
  const void** v_x_col_ptr = p_x_data->v_col_ptr;
  const void** v_y_col_ptr = p_y_data->v_col_ptr;

  // df-cols should already be flattened
  for (r_ssize col = 0; col < n_col; ++col) {
    if (!p_col_equal_na_equal(v_x_col_ptr[col], i, v_y_col_ptr[col], j, v_col_type[col])) {
      return false;
    }
  }

  return true;
}

// -----------------------------------------------------------------------------

static inline int lgl_equal_na_propagate(int x, int y) {
  if (lgl_is_missing(x) || lgl_is_missing(y)) {
    return NA_LOGICAL;
  } else {
    return lgl_equal_na_equal(x, y);
  }
}
static inline int int_equal_na_propagate(int x, int y) {
  if (int_is_missing(x) || int_is_missing(y)) {
    return NA_LOGICAL;
  } else {
    return int_equal_na_equal(x, y);
  }
}
static inline int dbl_equal_na_propagate(double x, double y) {
  if (dbl_is_missing(x) || dbl_is_missing(y)) {
    return NA_LOGICAL;
  } else {
    // Faster than `dbl_equal_na_equal()`,
    // which has unneeded missing value checks
    return x == y;
  }
}
static inline int cpl_equal_na_propagate(Rcomplex x, Rcomplex y) {
  int real_equal = dbl_equal_na_propagate(x.r, y.r);
  int imag_equal = dbl_equal_na_propagate(x.i, y.i);

  if (real_equal == NA_LOGICAL || imag_equal == NA_LOGICAL) {
    return NA_LOGICAL;
  } else {
    return real_equal && imag_equal;
  }
}
static inline int chr_equal_na_propagate(SEXP x, SEXP y) {
  if (chr_is_missing(x) || chr_is_missing(y)) {
    return NA_LOGICAL;
  } else {
    return chr_equal_na_equal(x, y);
  }
}
static inline int raw_equal_na_propagate(Rbyte x, Rbyte y) {
  return raw_equal_na_equal(x, y);
}
static inline int list_equal_na_propagate(SEXP x, SEXP y) {
  if (list_is_missing(x) || list_is_missing(y)) {
    return NA_LOGICAL;
  } else {
    return list_equal_na_equal(x, y);
  }
}

// -----------------------------------------------------------------------------

#define P_EQUAL_NA_PROPAGATE(CTYPE, EQUAL_NA_PROPAGATE) do {                   \
  return EQUAL_NA_PROPAGATE(((const CTYPE*) p_x)[i], ((const CTYPE*) p_y)[j]); \
} while (0)

static r_no_return
inline int p_nil_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  r_stop_internal("Can't compare NULL for equality.");
}
static inline int p_lgl_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_PROPAGATE(int, lgl_equal_na_propagate);
}
static inline int p_int_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_PROPAGATE(int, int_equal_na_propagate);
}
static inline int p_dbl_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_PROPAGATE(double, dbl_equal_na_propagate);
}
static inline int p_cpl_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_PROPAGATE(Rcomplex, cpl_equal_na_propagate);
}
static inline int p_chr_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_PROPAGATE(SEXP, chr_equal_na_propagate);
}
static inline int p_raw_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_PROPAGATE(Rbyte, raw_equal_na_propagate);
}
static inline int p_list_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_NA_PROPAGATE(SEXP, list_equal_na_propagate);
}

#undef P_EQUAL_NA_PROPAGATE

static inline bool p_equal_na_propagate(const void* p_x,
                                        r_ssize i,
                                        const void* p_y,
                                        r_ssize j,
                                        const enum vctrs_type type) {
  switch (type) {
  case VCTRS_TYPE_logical: return p_lgl_equal_na_propagate(p_x, i, p_y, j);
  case VCTRS_TYPE_integer: return p_int_equal_na_propagate(p_x, i, p_y, j);
  case VCTRS_TYPE_double: return p_dbl_equal_na_propagate(p_x, i, p_y, j);
  case VCTRS_TYPE_complex: return p_cpl_equal_na_propagate(p_x, i, p_y, j);
  case VCTRS_TYPE_character: return p_chr_equal_na_propagate(p_x, i, p_y, j);
  case VCTRS_TYPE_raw: return p_raw_equal_na_propagate(p_x, i, p_y, j);
  case VCTRS_TYPE_list: return p_list_equal_na_propagate(p_x, i, p_y, j);
  default: stop_unimplemented_vctrs_type("p_equal_na_propagate", type);
  }
}

// -----------------------------------------------------------------------------

#endif
