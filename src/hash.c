#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdint.h>
#include <stdbool.h>

// boost::hash_combine from https://stackoverflow.com/questions/35985960
int32_t hash_combine(int x, int y) {
  return x ^ y + 0x9e3779b9 + (x << 6) + (x >> 2);
}

// same approach as java 7
// https://docs.oracle.com/javase/7/docs/api/java/lang/Double.html#hashCode()
int32_t hash_double(double x) {
  union {
    double d;
    uint32_t i[2];
  } value;
  value.d = x;

  return value.i[0] ^ value.i[1];
}

// https://github.com/attractivechaos/klib/blob/master/khash.h#L385
int32_t hash_int64(int64_t x) {
  return x >> 33 ^ x ^ x << 11;
}

int32_t hash_vector(SEXP x);

int32_t hash_scalar(SEXP x, R_len_t i) {
  switch(TYPEOF(x)) {
  case LGLSXP:
    return LOGICAL(x)[i];
  case INTSXP:
    return INTEGER(x)[i];
  case REALSXP: {
    double val = REAL(x)[i];
    // Hash all NAs and NaNs to same value (i.e. ignoring significand)
    if (R_IsNA(val))
      val = NA_REAL;
    else if (R_IsNaN(val))
      val = R_NaN;

    return hash_double(val);
  }
  case STRSXP: {
    // currently assuming 64-bit pointer size
    return hash_int64((intptr_t) STRING_ELT(x, i));
  }
  case VECSXP: {
    return hash_vector(VECTOR_ELT(x, i));
  }

  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

int32_t hash_vector(SEXP x) {
  R_len_t n = Rf_length(x);
  int32_t hash = 0;

  for (R_len_t i = 0; i < n; ++i) {
    hash = hash_combine(hash, hash_scalar(x, i));
  }

  return hash;
}

// R interface -----------------------------------------------------------------

SEXP vctrs_hash(SEXP x) {
  R_len_t n = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));

  int32_t* pOut = INTEGER(out);
  for (R_len_t i = 0; i < n; ++i) {
    pOut[i] = hash_scalar(x, i);
  }

  UNPROTECT(1);
  return out;
}

SEXP vctrs_hash_vector(SEXP x) {
  return Rf_ScalarInteger(hash_vector(x));
}
