#ifndef VCTRS_MISSING_H
#define VCTRS_MISSING_H

#include "vctrs-core.h"
#include "utils.h"

// -----------------------------------------------------------------------------

r_obj* vec_equal_na(r_obj* x);

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

static inline
bool p_is_missing(const void* p_x,
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

#endif
