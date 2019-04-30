#include "vctrs.h"
#include "utils.h"


struct cow {
  SEXP obj;
  const PROTECT_INDEX i;
};

struct cow PROTECT_COW(SEXP x) {
  PROTECT_INDEX pi;
  PROTECT_WITH_INDEX(x, &pi);

  struct cow cow = { x, pi };
  return cow;
}

void cow_maybe_copy(struct cow* cow) {
  if (MAYBE_REFERENCED(cow->obj)) {
    cow->obj = Rf_shallow_duplicate(cow->obj);
    REPROTECT(cow->obj, cow->i);
  }
}


static SEXP as_minimal_names(struct cow* cow_names) {
  SEXP names = cow_names->obj;

  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector");
  }

  R_len_t i = 0;
  R_len_t n = Rf_length(names);
  SEXP* ptr = STRING_PTR(names);

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      break;
    }
  }
  if (i == n) {
    return names;
  }

  cow_maybe_copy(cow_names);
  names = cow_names->obj;

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      SET_STRING_ELT(names, i, vctrs_shared_empty_str_elt);
    }
  }

  return names;
}

SEXP vctrs_as_minimal_names(SEXP names) {
  struct cow cow_names = PROTECT_COW(names);
  SEXP out = as_minimal_names(&cow_names);

  UNPROTECT(1);
  return out;
}

SEXP vec_names(SEXP x) {
  if (Rf_inherits(x, "data.frame")) {
    return R_NilValue;
  }
  if (vec_dim(x) == 1) {
    return r_names(x);
  }

  SEXP dimnames = PROTECT(Rf_getAttrib(x, R_DimNamesSymbol));
  if (dimnames == R_NilValue || Rf_length(dimnames) < 1) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP out = VECTOR_ELT(dimnames, 0);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_minimal_names(SEXP x) {
  SEXP names = PROTECT(vec_names(x));

  if (names == R_NilValue) {
    UNPROTECT(1);
    return Rf_allocVector(STRSXP, vec_size(x));
  }

  struct cow cow_names = PROTECT_COW(names);
  names = as_minimal_names(&cow_names);

  UNPROTECT(2);
  return names;
}
