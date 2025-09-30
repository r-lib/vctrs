#ifndef VCTRS_HASH_H
#define VCTRS_HASH_H

#include "vctrs-core.h"

#define HASH_MISSING 1

// ----------------------------------------------------------------------------
// Vector

void vec_hash_fill(r_obj* x, r_ssize size, bool na_equal, uint32_t* v_out);

// ----------------------------------------------------------------------------
// Object

// Must be exposed for `list_hash_scalar()`
uint32_t obj_hash(r_obj* x);

// ----------------------------------------------------------------------------
// Hash utilities

// boost::hash_combine from https://stackoverflow.com/questions/35985960
static inline uint32_t hash_combine(uint32_t x, uint32_t y) {
  return x ^ (y + 0x9e3779b9 + (x << 6) + (x >> 2));
}

// 32-bit mixer from murmurhash
// https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp#L68
static inline uint32_t uint32_hash(uint32_t x) {
  x ^= x >> 16;
  x *= 0x85ebca6b;
  x ^= x >> 13;
  x *= 0xc2b2ae35;
  x ^= x >> 16;

  return x;
}

// 64-bit mixer from murmurhash
// https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp#L81
static inline uint32_t uint64_hash(uint64_t x) {
  x ^= x >> 33;
  x *= UINT64_C(0xff51afd7ed558ccd);
  x ^= x >> 33;
  x *= UINT64_C(0xc4ceb9fe1a85ec53);
  x ^= x >> 33;
  return x;
}

// ----------------------------------------------------------------------------
// Scalars

static inline uint32_t lgl_hash_scalar(int x) {
  return uint32_hash(x);
}

static inline uint32_t int_hash_scalar(int x) {
  return uint32_hash(x);
}

static inline uint32_t dbl_hash_scalar(double x) {
  // Seems like something designed specifically for doubles should work better
  // but I haven't been able to find anything

  // Hash all NAs and NaNs to same value (i.e. ignoring significand)
  double value;
  switch (dbl_classify(x)) {
  case VCTRS_DBL_number: value = x; break;
  case VCTRS_DBL_missing: value = NA_REAL; break;
  case VCTRS_DBL_nan: value = R_NaN; break;
  default: r_stop_unreachable();
  }

  // Treat positive/negative 0 as equivalent
  if (value == 0.0) {
    value = 0.0;
  }

  union {
    double d;
    uint64_t i;
  } value_union;

  value_union.d = value;
  return uint64_hash(value_union.i);
}

static inline uint32_t cpl_hash_scalar(r_complex x) {
  uint32_t hash = 0;
  hash = hash_combine(hash, dbl_hash_scalar(x.r));
  hash = hash_combine(hash, dbl_hash_scalar(x.i));
  return hash;
}

static inline uint32_t raw_hash_scalar(Rbyte x) {
  return uint32_hash(x);
}

static inline uint32_t chr_hash_scalar(r_obj* x) {
  return uint64_hash((uintptr_t) x);
}

static inline uint32_t list_hash_scalar(r_obj* x) {
  return obj_hash(x);
}

#endif
