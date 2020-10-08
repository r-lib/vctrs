#ifndef VCTRS_EQUAL_H
#define VCTRS_EQUAL_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

static inline bool lgl_is_missing(int x) {
  return x == NA_LOGICAL;
}
static inline bool int_is_missing(int x) {
  return x == NA_INTEGER;
}
static inline bool dbl_is_missing(double x) {
  return isnan(x);
}
static inline bool cpl_is_missing(Rcomplex x) {
  return dbl_is_missing(x.r) || dbl_is_missing(x.i);
}
static inline bool chr_is_missing(SEXP x) {
  return x == NA_STRING;
}
static inline bool raw_is_missing(Rbyte x) {
  return false;
}
static inline bool list_is_missing(SEXP x) {
  return x == R_NilValue;
}

// -----------------------------------------------------------------------------

#define P_IS_MISSING(CTYPE, IS_MISSING) do {   \
  return IS_MISSING(((const CTYPE*) p_x)[i]);  \
} while (0)

static inline bool p_nil_is_missing(const void* p_x, r_ssize i) {
  stop_internal("p_nil_is_missing", "Can't check NULL for missingness.");
}
static inline bool p_lgl_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(int, lgl_is_missing);
}
static inline bool p_int_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(int, int_is_missing);
}
static inline bool p_dbl_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(double, dbl_is_missing);
}
static inline bool p_cpl_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(Rcomplex, cpl_is_missing);
}
static inline bool p_chr_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(SEXP, chr_is_missing);
}
static inline bool p_raw_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(Rbyte, raw_is_missing);
}
static inline bool p_list_is_missing(const void* p_x, r_ssize i) {
  P_IS_MISSING(SEXP, list_is_missing);
}

#undef P_IS_MISSING

static inline bool p_is_missing(const void* p_x,
                                r_ssize i,
                                const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_logical: return p_lgl_is_missing(p_x, i);
  case vctrs_type_integer: return p_int_is_missing(p_x, i);
  case vctrs_type_double: return p_dbl_is_missing(p_x, i);
  case vctrs_type_complex: return p_cpl_is_missing(p_x, i);
  case vctrs_type_character: return p_chr_is_missing(p_x, i);
  case vctrs_type_raw: return p_raw_is_missing(p_x, i);
  case vctrs_type_list: return p_list_is_missing(p_x, i);
  default: stop_unimplemented_vctrs_type("p_is_missing", type);
  }
}

// -----------------------------------------------------------------------------

static inline int lgl_equal_na_equal(int x, int y) {
  return x == y;
}
static inline int int_equal_na_equal(int x, int y) {
  return x == y;
}
static inline int dbl_equal_na_equal(double x, double y) {
  switch (dbl_classify(x)) {
  case vctrs_dbl_number: break;
  case vctrs_dbl_missing: return dbl_classify(y) == vctrs_dbl_missing;
  case vctrs_dbl_nan: return dbl_classify(y) == vctrs_dbl_nan;
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
  return equal_object(x, y);
}

// -----------------------------------------------------------------------------

#define P_EQUAL_NA_EQUAL(CTYPE, EQUAL_NA_EQUAL) do {                       \
  return EQUAL_NA_EQUAL(((const CTYPE*) p_x)[i], ((const CTYPE*) p_y)[j]); \
} while (0)

static inline int p_nil_equal_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  stop_internal("p_nil_equal_na_equal", "Can't compare NULL for equality.");
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

static inline bool p_equal_na_equal(const void* p_x,
                                    r_ssize i,
                                    const void* p_y,
                                    r_ssize j,
                                    const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_logical: return p_lgl_equal_na_equal(p_x, i, p_y, j);
  case vctrs_type_integer: return p_int_equal_na_equal(p_x, i, p_y, j);
  case vctrs_type_double: return p_dbl_equal_na_equal(p_x, i, p_y, j);
  case vctrs_type_complex: return p_cpl_equal_na_equal(p_x, i, p_y, j);
  case vctrs_type_character: return p_chr_equal_na_equal(p_x, i, p_y, j);
  case vctrs_type_raw: return p_raw_equal_na_equal(p_x, i, p_y, j);
  case vctrs_type_list: return p_list_equal_na_equal(p_x, i, p_y, j);
  default: stop_unimplemented_vctrs_type("p_equal_na_equal", type);
  }
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

static inline int p_nil_equal_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  stop_internal("p_nil_equal_na_propagate", "Can't compare NULL for equality.");
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
  case vctrs_type_logical: return p_lgl_equal_na_propagate(p_x, i, p_y, j);
  case vctrs_type_integer: return p_int_equal_na_propagate(p_x, i, p_y, j);
  case vctrs_type_double: return p_dbl_equal_na_propagate(p_x, i, p_y, j);
  case vctrs_type_complex: return p_cpl_equal_na_propagate(p_x, i, p_y, j);
  case vctrs_type_character: return p_chr_equal_na_propagate(p_x, i, p_y, j);
  case vctrs_type_raw: return p_raw_equal_na_propagate(p_x, i, p_y, j);
  case vctrs_type_list: return p_list_equal_na_propagate(p_x, i, p_y, j);
  default: stop_unimplemented_vctrs_type("p_equal_na_propagate", type);
  }
}

// -----------------------------------------------------------------------------

#endif
