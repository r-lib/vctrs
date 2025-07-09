#ifndef VCTRS_MISSING_H
#define VCTRS_MISSING_H

#include "vctrs-core.h"
#include "poly-op.h"

// -----------------------------------------------------------------------------

r_obj* vec_detect_missing(r_obj* x);
bool vec_any_missing(r_obj* x);
r_ssize vec_first_missing(r_obj* x);

// -----------------------------------------------------------------------------

static inline
bool lgl_is_missing(int x) {
  return x == r_globals.na_int;
}
static inline
bool int_is_missing(int x) {
  return x == r_globals.na_int;
}
static inline
bool dbl_is_missing(double x) {
  return isnan(x);
}
static inline
bool cpl_is_missing(r_complex x) {
  return dbl_is_missing(x.r) || dbl_is_missing(x.i);
}
static inline
bool chr_is_missing(r_obj* x) {
  return x == r_globals.na_str;
}
static inline
bool raw_is_missing(unsigned char x) {
  return false;
}
static inline
bool list_is_missing(r_obj* x) {
  return x == r_null;
}

// -----------------------------------------------------------------------------

#define P_IS_MISSING(CTYPE, IS_MISSING) do {   \
  return IS_MISSING(((CTYPE const*) p_x)[i]);  \
} while (0)

static r_no_return inline
bool p_nil_is_missing(const void* p_x, r_ssize i) {
  r_stop_internal("Can't check NULL for missingness.");
}
static inline
bool p_lgl_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(int, lgl_is_missing);
}
static inline
bool p_int_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(int, int_is_missing);
}
static inline
bool p_dbl_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(double, dbl_is_missing);
}
static inline
bool p_cpl_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(r_complex, cpl_is_missing);
}
static inline
bool p_chr_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(r_obj*, chr_is_missing);
}
static inline
bool p_raw_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(unsigned char, raw_is_missing);
}
static inline
bool p_list_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(r_obj*, list_is_missing);
}

#undef P_IS_MISSING

// No support for df-cols, as they should be flattened
static inline
bool p_col_is_missing(
  const void* p_x,
  r_ssize i,
  const enum vctrs_type type
) {
  switch (type) {
  case VCTRS_TYPE_logical: return p_lgl_is_missing(p_x, i);
  case VCTRS_TYPE_integer: return p_int_is_missing(p_x, i);
  case VCTRS_TYPE_double: return p_dbl_is_missing(p_x, i);
  case VCTRS_TYPE_complex: return p_cpl_is_missing(p_x, i);
  case VCTRS_TYPE_character: return p_chr_is_missing(p_x, i);
  case VCTRS_TYPE_raw: return p_raw_is_missing(p_x, i);
  case VCTRS_TYPE_list: return p_list_is_missing(p_x, i);
  default: stop_unimplemented_vctrs_type("p_col_is_missing", type);
  }
}

static inline
bool p_df_is_missing(const void* p_x, r_ssize i) {
  struct poly_df_data* p_x_data = (struct poly_df_data*) p_x;

  enum vctrs_type* v_col_type = p_x_data->v_col_type;
  const void** v_col_ptr = p_x_data->v_col_ptr;
  r_ssize n_col = p_x_data->n_col;

  // df-cols should already be flattened
  for (r_ssize col = 0; col < n_col; ++col) {
    if (!p_col_is_missing(v_col_ptr[col], i, v_col_type[col])) {
      return false;
    }
  }

  return true;
}

// -----------------------------------------------------------------------------

static r_no_return inline
bool p_nil_is_incomplete(const void* p_x, r_ssize i) {
  p_nil_is_missing(p_x, i);
}
static inline
bool p_lgl_is_incomplete(const void* p_x, r_ssize i) {
  return p_lgl_is_missing(p_x, i);
}
static inline
bool p_int_is_incomplete(const void* p_x, r_ssize i) {
  return p_int_is_missing(p_x, i);
}
static inline
bool p_dbl_is_incomplete(const void* p_x, r_ssize i) {
  return p_dbl_is_missing(p_x, i);
}
static inline
bool p_cpl_is_incomplete(const void* p_x, r_ssize i) {
  return p_cpl_is_missing(p_x, i);
}
static inline
bool p_chr_is_incomplete(const void* p_x, r_ssize i) {
  return p_chr_is_missing(p_x, i);
}
static inline
bool p_raw_is_incomplete(const void* p_x, r_ssize i) {
  return p_raw_is_missing(p_x, i);
}
static inline
bool p_list_is_incomplete(const void* p_x, r_ssize i) {
  return p_list_is_missing(p_x, i);
}

static inline
bool p_df_is_incomplete(const void* p_x, r_ssize i) {
  struct poly_df_data* p_x_data = (struct poly_df_data*) p_x;

  enum vctrs_type* v_col_type = p_x_data->v_col_type;
  const void** v_col_ptr = p_x_data->v_col_ptr;
  r_ssize n_col = p_x_data->n_col;

  // df-cols should already be flattened,
  // so we only need missingness of each column, not completeness
  for (r_ssize col = 0; col < n_col; ++col) {
    if (p_col_is_missing(v_col_ptr[col], i, v_col_type[col])) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------

#endif
