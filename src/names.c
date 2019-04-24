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

SEXP vctrs_minimal_names(SEXP x) {
  SEXP names = Rf_getAttrib(x, R_NamesSymbol);

  if (names == R_NilValue) {
    return Rf_allocVector(STRSXP, Rf_length(x));
  }

  struct cow cow_names = PROTECT_COW(names);
  names = as_minimal_names(&cow_names);

  UNPROTECT(1);
  return names;
}
