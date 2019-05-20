#include "vctrs.h"

// boost::hash_combine from https://stackoverflow.com/questions/35985960
int32_t hash_combine(int x, int y) {
  return x ^ (y + 0x9e3779b9 + (x << 6) + (x >> 2));
}

// 32-bit mixer from murmurhash
// https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp#L68
int32_t hash_int32(int32_t x) {
  x ^= x >> 16;
  x *= 0x85ebca6b;
  x ^= x >> 13;
  x *= 0xc2b2ae35;
  x ^= x >> 16;

  return x;
}

// 64-bit mixer from murmurhash
// https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp#L81
int32_t hash_int64(int64_t x) {
  x ^= x >> 33;
  x *= UINT64_C(0xff51afd7ed558ccd);
  x ^= x >> 33;
  x *= UINT64_C(0xc4ceb9fe1a85ec53);
  x ^= x >> 33;
  return x;
}

// Seems like something designed specificaly for doubles should work better
// but I haven't been able to find anything
int32_t hash_double(double x) {
  union {
    double d;
    uint64_t i;
  } value;
  value.d = x;

  return hash_int64(value.i);
}


// Hashing scalars -----------------------------------------------------

static int32_t lgl_hash_scalar(const int* x);
static int32_t int_hash_scalar(const int* x);
static int32_t dbl_hash_scalar(const double* x);
static int32_t cpl_hash_scalar(const Rcomplex* x);
static int32_t chr_hash_scalar(const SEXP* x);
static int32_t raw_hash_scalar(const Rbyte* x);
static int32_t df_hash_scalar(SEXP x, R_len_t i);
static int32_t list_hash_scalar(SEXP x, R_len_t i);

// [[ include("vctrs.h") ]]
int32_t hash_scalar(SEXP x, R_len_t i) {
  switch(TYPEOF(x)) {
  case LGLSXP: return lgl_hash_scalar(LOGICAL(x) + i);
  case INTSXP: return int_hash_scalar(INTEGER(x) + i);
  case REALSXP: return dbl_hash_scalar(REAL(x) + i);
  case CPLXSXP: return cpl_hash_scalar(COMPLEX(x) + i);
  case STRSXP: return chr_hash_scalar(STRING_PTR(x) + i);
  case RAWSXP: return raw_hash_scalar(RAW(x) + i);
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_hash_scalar(x, i);
    } else {
      return list_hash_scalar(x, i);
    }
  }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP vctrs_hash(SEXP x) {
  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));

  int32_t* pOut = INTEGER(out);
  for (R_len_t i = 0; i < n; ++i) {
    pOut[i] = hash_scalar(x, i);
  }

  UNPROTECT(1);
  return out;
}

static int32_t lgl_hash_scalar(const int* x) {
  return hash_int32(*x);
}
static int32_t int_hash_scalar(const int* x) {
  return hash_int32(*x);
}
static int32_t dbl_hash_scalar(const double* x) {
  double val = *x;
  // Hash all NAs and NaNs to same value (i.e. ignoring significand)
  if (R_IsNA(val)) {
    val = NA_REAL;
  } else if (R_IsNaN(val)) {
    val = R_NaN;
  }
  return hash_double(val);
}
static int32_t cpl_hash_scalar(const Rcomplex* x) {
  Rf_error("Hashing is not implemented for complex vectors.");
}
static int32_t chr_hash_scalar(const SEXP* x) {
  return hash_object(*x);
}
static int32_t raw_hash_scalar(const Rbyte* x) {
  Rf_error("Hashing is not implemented for raw vectors.");
}

static int32_t df_hash_scalar(SEXP x, R_len_t i) {
  uint32_t hash = 0;
  R_len_t p = Rf_length(x);

  for (R_len_t j = 0; j < p; ++j) {
    SEXP col = VECTOR_ELT(x, j);
    hash = hash_combine(hash, hash_scalar(col, i));
  }

  return hash;
}

static int32_t list_hash_scalar(SEXP x, R_len_t i) {
  return hash_object(VECTOR_ELT(x, i));
}


// Hashing objects -----------------------------------------------------

static int32_t lgl_hash(SEXP x);
static int32_t int_hash(SEXP x);
static int32_t dbl_hash(SEXP x);
static int32_t chr_hash(SEXP x);
static int32_t list_hash(SEXP x);
static int32_t node_hash(SEXP x);
static int32_t fn_hash(SEXP x);

int32_t hash_object(SEXP x) {
  switch(TYPEOF(x)) {
  case NILSXP: return 0;
  case LGLSXP: return lgl_hash(x);
  case INTSXP: return int_hash(x);
  case REALSXP: return dbl_hash(x);
  case STRSXP: return chr_hash(x);
  case VECSXP: return list_hash(x);
  case DOTSXP:
  case LANGSXP:
  case LISTSXP:
  case BCODESXP: return node_hash(x);
  case CLOSXP: return fn_hash(x);
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP: return hash_int64((intptr_t) x);
  default: Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// [[ register() ]]
SEXP vctrs_hash_object(SEXP x) {
  return Rf_ScalarInteger(hash_object(x));
}


#define HASH(CTYPE, CONST_DEREF, HASHER)        \
  int32_t hash = 0;                             \
  R_len_t n = Rf_length(x);                     \
  const CTYPE* p = CONST_DEREF(x);              \
                                                \
  for (R_len_t i = 0; i < n; ++i, ++p) {        \
    hash = hash_combine(hash, HASHER(p));       \
  }                                             \
                                                \
  return hash

static int32_t lgl_hash(SEXP x) {
  HASH(int, LOGICAL_RO, lgl_hash_scalar);
}
static int32_t int_hash(SEXP x) {
  HASH(int, INTEGER_RO, int_hash_scalar);
}
static int32_t dbl_hash(SEXP x) {
  HASH(double, REAL_RO, dbl_hash_scalar);
}
static int32_t chr_hash(SEXP x) {
  HASH(SEXP, STRING_PTR_RO, chr_hash_scalar);
}

#undef HASH


#define HASH_BARRIER(GET, HASHER)                       \
  int32_t hash = 0;                                     \
  R_len_t n = Rf_length(x);                             \
                                                        \
  for (R_len_t i = 0; i < n; ++i) {                     \
    hash = hash_combine(hash, HASHER(GET(x, i)));       \
  }                                                     \
                                                        \
  return hash

static int32_t list_hash(SEXP x) {
  HASH_BARRIER(VECTOR_ELT, hash_object);
}

#undef HASH_BARRIER


static int32_t node_hash(SEXP x) {
  int32_t hash = 0;
  hash = hash_combine(hash, hash_object(CAR(x)));
  hash = hash_combine(hash, hash_object(CDR(x)));
  return hash;
}

static int32_t fn_hash(SEXP x) {
  int32_t hash = 0;
  hash = hash_combine(hash, hash_object(BODY(x)));
  hash = hash_combine(hash, hash_object(CLOENV(x)));
  hash = hash_combine(hash, hash_object(FORMALS(x)));
  return hash;
}
