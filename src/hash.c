#include "vctrs.h"
#include "utils.h"

// boost::hash_combine from https://stackoverflow.com/questions/35985960
static uint32_t hash_combine(uint32_t x, uint32_t y) {
  return x ^ (y + 0x9e3779b9 + (x << 6) + (x >> 2));
}

// 32-bit mixer from murmurhash
// https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp#L68
static uint32_t hash_int32(uint32_t x) {
  x ^= x >> 16;
  x *= 0x85ebca6b;
  x ^= x >> 13;
  x *= 0xc2b2ae35;
  x ^= x >> 16;

  return x;
}

// 64-bit mixer from murmurhash
// https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp#L81
static uint32_t hash_int64(int64_t x) {
  x ^= x >> 33;
  x *= UINT64_C(0xff51afd7ed558ccd);
  x ^= x >> 33;
  x *= UINT64_C(0xc4ceb9fe1a85ec53);
  x ^= x >> 33;
  return x;
}

// Seems like something designed specificaly for doubles should work better
// but I haven't been able to find anything
static uint32_t hash_double(double x) {
  // Treat positive/negative 0 as equivalent
  if (x == 0.0) {
    x = 0.0;
  }

  union {
    double d;
    uint64_t i;
  } value;
  value.d = x;

  return hash_int64(value.i);
}

static uint32_t hash_char(SEXP x) {
  return hash_int64((intptr_t) x);
}

// Hashing scalars -----------------------------------------------------

static uint32_t lgl_hash_scalar(const int* x);
static uint32_t int_hash_scalar(const int* x);
static uint32_t dbl_hash_scalar(const double* x);
static uint32_t cpl_hash_scalar(const Rcomplex* x);
static uint32_t chr_hash_scalar(const SEXP* x);
static uint32_t raw_hash_scalar(const Rbyte* x);
static uint32_t list_hash_scalar(SEXP x, R_len_t i);


static uint32_t lgl_hash_scalar(const int* x) {
  return hash_int32(*x);
}
static uint32_t int_hash_scalar(const int* x) {
  return hash_int32(*x);
}
static uint32_t dbl_hash_scalar(const double* x) {
  double val = *x;

  // Hash all NAs and NaNs to same value (i.e. ignoring significand)
  switch (dbl_classify(val)) {
  case vctrs_dbl_number: break;
  case vctrs_dbl_missing: val = NA_REAL; break;
  case vctrs_dbl_nan: val = R_NaN; break;
  }

  return hash_double(val);
}
static uint32_t cpl_hash_scalar(const Rcomplex* x) {
  uint32_t hash = 0;
  hash = hash_combine(hash, dbl_hash_scalar(&x->r));
  hash = hash_combine(hash, dbl_hash_scalar(&x->i));
  return hash;
}
static uint32_t chr_hash_scalar(const SEXP* x) {
  return hash_char(*x);
}
static uint32_t raw_hash_scalar(const Rbyte* x) {
  return hash_int32(*x);
}

static uint32_t list_hash_scalar(SEXP x, R_len_t i) {
  return hash_object(VECTOR_ELT(x, i));
}

// Hashing objects -----------------------------------------------------

static uint32_t lgl_hash(SEXP x);
static uint32_t int_hash(SEXP x);
static uint32_t dbl_hash(SEXP x);
static uint32_t chr_hash(SEXP x);
static uint32_t list_hash(SEXP x);
static uint32_t node_hash(SEXP x);
static uint32_t fn_hash(SEXP x);
static uint32_t sexp_hash(SEXP x);

uint32_t hash_object(SEXP x) {
  uint32_t hash = sexp_hash(x);

  SEXP attrib = ATTRIB(x);
  if (attrib != R_NilValue) {
    hash = hash_combine(hash, hash_object(attrib));
  }

  return hash;
}

// [[ register() ]]
SEXP vctrs_hash_object(SEXP x) {
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, sizeof(uint32_t)));
  uint32_t hash = 0;
  hash = hash_combine(hash, hash_object(x));
  memcpy(RAW(out), &hash, sizeof(uint32_t));
  UNPROTECT(1);
  return out;
}


static uint32_t sexp_hash(SEXP x) {
  switch(TYPEOF(x)) {
  case NILSXP: return 0;
  case LGLSXP: return lgl_hash(x);
  case INTSXP: return int_hash(x);
  case REALSXP: return dbl_hash(x);
  case STRSXP: return chr_hash(x);
  case EXPRSXP:
  case VECSXP: return list_hash(x);
  case DOTSXP:
  case LANGSXP:
  case LISTSXP:
  case BCODESXP: return node_hash(x);
  case CLOSXP: return fn_hash(x);
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case ENVSXP:
  case EXTPTRSXP: return hash_int64((intptr_t) x);
  default: Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

#define HASH(CTYPE, CONST_DEREF, HASHER)        \
  uint32_t hash = 0;                            \
  R_len_t n = Rf_length(x);                     \
  const CTYPE* p = CONST_DEREF(x);              \
                                                \
  for (R_len_t i = 0; i < n; ++i, ++p) {        \
    hash = hash_combine(hash, HASHER(p));       \
  }                                             \
                                                \
  return hash

static uint32_t lgl_hash(SEXP x) {
  HASH(int, LOGICAL_RO, lgl_hash_scalar);
}
static uint32_t int_hash(SEXP x) {
  HASH(int, INTEGER_RO, int_hash_scalar);
}
static uint32_t dbl_hash(SEXP x) {
  HASH(double, REAL_RO, dbl_hash_scalar);
}
static uint32_t chr_hash(SEXP x) {
  HASH(SEXP, STRING_PTR_RO, chr_hash_scalar);
}

#undef HASH


#define HASH_BARRIER(GET, HASHER)                       \
  uint32_t hash = 0;                                    \
  R_len_t n = Rf_length(x);                             \
                                                        \
  for (R_len_t i = 0; i < n; ++i) {                     \
    hash = hash_combine(hash, HASHER(GET(x, i)));       \
  }                                                     \
                                                        \
  return hash

static uint32_t list_hash(SEXP x) {
  HASH_BARRIER(VECTOR_ELT, hash_object);
}

#undef HASH_BARRIER


static uint32_t node_hash(SEXP x) {
  uint32_t hash = 0;
  hash = hash_combine(hash, hash_object(CAR(x)));
  hash = hash_combine(hash, hash_object(CDR(x)));
  return hash;
}

static uint32_t fn_hash(SEXP x) {
  uint32_t hash = 0;
  hash = hash_combine(hash, hash_object(BODY(x)));
  hash = hash_combine(hash, hash_object(CLOENV(x)));
  hash = hash_combine(hash, hash_object(FORMALS(x)));
  return hash;
}


// Fill hash array -----------------------------------------------------

static void lgl_hash_fill(uint32_t* p, R_len_t size, SEXP x);
static void int_hash_fill(uint32_t* p, R_len_t size, SEXP x);
static void dbl_hash_fill(uint32_t* p, R_len_t size, SEXP x);
static void cpl_hash_fill(uint32_t* p, R_len_t size, SEXP x);
static void chr_hash_fill(uint32_t* p, R_len_t size, SEXP x);
static void raw_hash_fill(uint32_t* p, R_len_t size, SEXP x);
static void list_hash_fill(uint32_t* p, R_len_t size, SEXP x);
static void df_hash_fill(uint32_t* p, R_len_t size, SEXP x);

// Not compatible with hash_scalar
// [[ include("vctrs.h") ]]
void hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  if (has_dim(x)) {
    // The conversion to data frame is only a stopgap, in the long
    // term, we'll hash arrays natively
    x = PROTECT(r_as_data_frame(x));
    hash_fill(p, size, x);
    UNPROTECT(1);
    return;
  }

  switch (TYPEOF(x)) {
  case LGLSXP: lgl_hash_fill(p, size, x); return;
  case INTSXP: int_hash_fill(p, size, x); return;
  case REALSXP: dbl_hash_fill(p, size, x); return;
  case CPLXSXP: cpl_hash_fill(p, size, x); return;
  case STRSXP: chr_hash_fill(p, size, x); return;
  case RAWSXP: raw_hash_fill(p, size, x); return;
  case VECSXP:
    if (is_data_frame(x)) {
      df_hash_fill(p, size, x);
    } else {
      list_hash_fill(p, size, x);
    }
    return;
  default:
    Rf_error("Internal error: Unsupported type %s in `hash_fill()`.", Rf_type2char(TYPEOF(x)));
  }
}

#define HASH_FILL(CTYPE, CONST_DEREF, HASHER)   \
  const CTYPE* xp = CONST_DEREF(x);             \
                                                \
  for (R_len_t i = 0; i < size; ++i, ++xp) {    \
    p[i] = hash_combine(p[i], HASHER(xp));      \
  }

static void lgl_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  HASH_FILL(int, LOGICAL_RO, lgl_hash_scalar);
}
static void int_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  HASH_FILL(int, INTEGER_RO, int_hash_scalar);
}
static void dbl_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  HASH_FILL(double, REAL_RO, dbl_hash_scalar);
}
static void cpl_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  HASH_FILL(Rcomplex, COMPLEX_RO, cpl_hash_scalar);
}
static void chr_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  HASH_FILL(SEXP, STRING_PTR_RO, chr_hash_scalar);
}
static void raw_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  HASH_FILL(Rbyte, RAW_RO, raw_hash_scalar);
}

#undef HASH_FILL


#define HASH_FILL_BARRIER(HASHER)               \
  for (R_len_t i = 0; i < size; ++i) {          \
    p[i] = hash_combine(p[i], HASHER(x, i));    \
  }

static void list_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  HASH_FILL_BARRIER(list_hash_scalar);
}

#undef HASH_FILL_BARRIER


static void df_hash_fill(uint32_t* p, R_len_t size, SEXP x) {
  R_len_t ncol = Rf_length(x);

  for (R_len_t i = 0; i < ncol; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    hash_fill(p, size, col);
  }
}

// [[ register() ]]
SEXP vctrs_hash(SEXP x) {
  x = PROTECT(vec_proxy_equal(x));

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, n * sizeof(uint32_t)));

  uint32_t* p = (uint32_t*) RAW(out);

  memset(p, 0, n * sizeof(uint32_t));
  hash_fill(p, n, x);

  UNPROTECT(2);
  return out;
}
