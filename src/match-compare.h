#ifndef VCTRS_MATCH_COMPARE_H
#define VCTRS_MATCH_COMPARE_H

#include "vctrs-core.h"
#include "poly-op.h"
#include "type-complex.h"

/*
 * These comparison operators are designed to match the comparison order
 * returned from:
 * `vec_order(x, direction = "asc", na_value = "smallest", nan_distinct = nan_distinct)`
 *
 * They are intended for internal use in `vec_joint_xtfrm()`, which uses that
 * exact setup to call `vec_order_info()`.
 *
 * In particular, double and complex types match the ordering results from
 * using `nan_distinct`. If `false`, they are treated equally. If `true`,
 * since this is ascending order and `NA` values are the smallest value, it
 * places `NA` before `NaN` followed by real numbers to match `vec_order()`.
 */

// -----------------------------------------------------------------------------

static inline
int lgl_order_compare_na_equal(int x, int y, bool nan_distinct) {
  return lgl_compare_na_equal(x, y);
}
static inline
int int_order_compare_na_equal(int x, int y, bool nan_distinct) {
  return int_compare_na_equal(x, y);
}
static inline
int dbl_order_compare_na_equal(double x, double y, bool nan_distinct) {
  enum vctrs_dbl x_class = dbl_classify(x);
  enum vctrs_dbl y_class = dbl_classify(y);

  switch (x_class) {
  case VCTRS_DBL_number: {
    switch (y_class) {
    case VCTRS_DBL_number: return dbl_compare_scalar(x, y);
    case VCTRS_DBL_missing: return 1;
    case VCTRS_DBL_nan: return 1;
    }
  }
  case VCTRS_DBL_missing: {
    switch (y_class) {
    case VCTRS_DBL_number: return -1;
    case VCTRS_DBL_missing: return 0;
    case VCTRS_DBL_nan: return nan_distinct ? -1 : 0;
    }
  }
  case VCTRS_DBL_nan: {
    switch (y_class) {
    case VCTRS_DBL_number: return -1;
    case VCTRS_DBL_missing: return nan_distinct ? 1 : 0;
    case VCTRS_DBL_nan: return 0;
    }
  }
  }

  r_stop_unreachable();
}
static inline
int cpl_order_compare_na_equal(r_complex x, r_complex y, bool nan_distinct) {
  x = cpl_normalise_missing(x);
  y = cpl_normalise_missing(y);

  const int cmp = dbl_order_compare_na_equal(x.r, y.r, nan_distinct);

  if (cmp == 0) {
    return dbl_order_compare_na_equal(x.i, y.i, nan_distinct);
  } else {
    return cmp;
  }
}
static inline
int chr_order_compare_na_equal(r_obj* x, r_obj* y, bool nan_distinct) {
  return chr_compare_na_equal(x, y);
}

// -----------------------------------------------------------------------------

#define P_ORDER_COMPARE_NA_EQUAL(CTYPE, ORDER_COMPARE_NA_EQUAL) do {                             \
  return ORDER_COMPARE_NA_EQUAL(((CTYPE const*) p_x)[i], ((CTYPE const*) p_y)[j], nan_distinct); \
} while (0)


static inline
int p_lgl_order_compare_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j, bool nan_distinct) {
  P_ORDER_COMPARE_NA_EQUAL(int, lgl_order_compare_na_equal);
}
static inline
int p_int_order_compare_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j, bool nan_distinct) {
  P_ORDER_COMPARE_NA_EQUAL(int, int_order_compare_na_equal);
}
static inline
int p_dbl_order_compare_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j, bool nan_distinct) {
  P_ORDER_COMPARE_NA_EQUAL(double, dbl_order_compare_na_equal);
}
static inline
int p_cpl_order_compare_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j, bool nan_distinct) {
  P_ORDER_COMPARE_NA_EQUAL(r_complex, cpl_order_compare_na_equal);
}
static inline
int p_chr_order_compare_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j, bool nan_distinct) {
  P_ORDER_COMPARE_NA_EQUAL(r_obj*, chr_order_compare_na_equal);
}


#undef P_ORDER_COMPARE_NA_EQUAL


static inline
int p_order_compare_na_equal(const void* p_x,
                             r_ssize i,
                             const void* p_y,
                             r_ssize j,
                             bool nan_distinct,
                             const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_logical: return p_lgl_order_compare_na_equal(p_x, i, p_y, j, nan_distinct);
  case vctrs_type_integer: return p_int_order_compare_na_equal(p_x, i, p_y, j, nan_distinct);
  case vctrs_type_double: return p_dbl_order_compare_na_equal(p_x, i, p_y, j, nan_distinct);
  case vctrs_type_complex: return p_cpl_order_compare_na_equal(p_x, i, p_y, j, nan_distinct);
  case vctrs_type_character: return p_chr_order_compare_na_equal(p_x, i, p_y, j, nan_distinct);
  default: stop_unimplemented_vctrs_type("p_order_compare_na_equal", type);
  }
}

static inline
int p_df_order_compare_na_equal(const void* x,
                                r_ssize i,
                                const void* y,
                                r_ssize j,
                                bool nan_distinct) {
  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;

  enum vctrs_type* v_col_type = x_data->v_col_type;
  const void** v_x_col_ptr = x_data->v_col_ptr;
  const void** v_y_col_ptr = y_data->v_col_ptr;

  // df-cols should already be flattened
  for (r_ssize col = 0; col < n_col; ++col) {
    int cmp = p_order_compare_na_equal(
      v_x_col_ptr[col], i,
      v_y_col_ptr[col], j,
      nan_distinct,
      v_col_type[col]
    );

    if (cmp == 0) {
      // Equal values for this column
      continue;
    }

    // Difference detected
    return cmp;
  }

  // All columns were equal
  return 0;
}

// -----------------------------------------------------------------------------

#endif
