#ifndef VCTRS_EQUAL_H
#define VCTRS_EQUAL_H

#include "vctrs.h"


// Storing pointed values on the stack helps performance for the
// `!na_equal` cases
static inline int lgl_equal_scalar(const int* x, const int* y, bool na_equal) {
  const int xi = *x;
  const int yj = *y;
  if (na_equal) {
    return xi == yj;
  } else {
    return (xi == NA_LOGICAL || yj == NA_LOGICAL) ? NA_LOGICAL : xi == yj;
  }
}

static inline int int_equal_scalar(const int* x, const int* y, bool na_equal) {
  const int xi = *x;
  const int yj = *y;
  if (na_equal) {
    return xi == yj;
  } else {
    return (xi == NA_INTEGER || yj == NA_INTEGER) ? NA_LOGICAL : xi == yj;
  }
}

static inline int dbl_equal_scalar(const double* x, const double* y, bool na_equal) {
  const double xi = *x;
  const double yj = *y;

  if (na_equal) {
    switch (dbl_classify(xi)) {
    case vctrs_dbl_number: break;
    case vctrs_dbl_missing: return dbl_classify(yj) == vctrs_dbl_missing;
    case vctrs_dbl_nan: return dbl_classify(yj) == vctrs_dbl_nan;
    }

    if (isnan(yj)) {
      return false;
    }
  } else {
    if (isnan(xi) || isnan(yj)) return NA_LOGICAL;
  }
  return xi == yj;
}

static inline int cpl_equal_scalar(const Rcomplex* x, const Rcomplex* y, bool na_equal) {
  int real_equal = dbl_equal_scalar(&x->r, &y->r, na_equal);
  int imag_equal = dbl_equal_scalar(&x->i, &y->i, na_equal);
  if (real_equal == NA_LOGICAL || imag_equal == NA_LOGICAL) {
    return NA_LOGICAL;
  } else {
    return real_equal && imag_equal;
  }
}

// UTF-8 translation is successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation fails purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static inline int chr_equal_scalar_impl(const SEXP x, const SEXP y) {
  if (x == y) {
    return 1;
  }

  if (Rf_getCharCE(x) != Rf_getCharCE(y)) {
    const void *vmax = vmaxget();
    int out = !strcmp(Rf_translateCharUTF8(x), Rf_translateCharUTF8(y));
    vmaxset(vmax);
    return out;
  }

  return 0;
}

static inline int chr_equal_scalar(const SEXP* x, const SEXP* y, bool na_equal) {
  const SEXP xi = *x;
  const SEXP yj = *y;
  if (na_equal) {
    return chr_equal_scalar_impl(xi, yj);
  } else {
    return (xi == NA_STRING || yj == NA_STRING) ? NA_LOGICAL : chr_equal_scalar_impl(xi, yj);
  }
}

static inline int list_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  const SEXP xi = VECTOR_ELT(x, i);
  const SEXP yj = VECTOR_ELT(y, j);

  if (na_equal) {
    return equal_object(xi, yj);
  } else {
    return (xi == R_NilValue || yj == R_NilValue) ? NA_LOGICAL : equal_object(xi, yj);
  }
}

static inline int raw_equal_scalar(const Rbyte* x, const Rbyte* y, bool na_equal) {
  // Raw vectors have no notion of missing value
  return *x == *y;
}

static inline int df_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal, int n_col) {
  for (int k = 0; k < n_col; ++k) {
    int eq = equal_scalar(VECTOR_ELT(x, k), i, VECTOR_ELT(y, k), j, na_equal);

    if (eq <= 0) {
      return eq;
    }
  }

  return true;
}



#endif
