#include "vctrs.h"
#include "utils.h"


struct cow {
  SEXP obj;
  PROTECT_INDEX i;
};

struct cow PROTECT_COW(SEXP x) {
  PROTECT_INDEX pi;
  PROTECT_WITH_INDEX(x, &pi);

  struct cow cow = { x, pi };
  return cow;
}

struct cow cow_maybe_copy(struct cow cow) {
  if (MAYBE_REFERENCED(cow.obj)) {
    cow.obj = Rf_shallow_duplicate(cow.obj);
    REPROTECT(cow.obj, cow.i);
  }
  return cow;
}

static struct cow as_minimal_names(struct cow cow_names) {
  SEXP names = cow_names.obj;

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
    return cow_names;
  }

  cow_names = cow_maybe_copy(cow_names);
  names = cow_names.obj;

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      SET_STRING_ELT(names, i, vctrs_shared_empty_str_elt);
    }
  }

  return cow_names;
}

SEXP vctrs_as_minimal_names(SEXP names) {
  struct cow cow_names = PROTECT_COW(names);
  cow_names = as_minimal_names(cow_names);

  UNPROTECT(1);
  return cow_names.obj;
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
  cow_names = as_minimal_names(cow_names);

  UNPROTECT(2);
  return cow_names.obj;
}


static SEXP names_iota(R_len_t n);

SEXP vctrs_unique_names(SEXP x) {
  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  if (names == R_NilValue) {
    UNPROTECT(1);
    return(names_iota(vec_size(x)));
  }

  UNPROTECT(1);
  return names;
}


// 3 leading '.' + 1 trailing '\0' + 24 characters
#define TOTAL_BUF_SIZE 28
#define FREE_BUF_SIZE 25

static SEXP names_iota(R_len_t n) {
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, n));

  char buf[TOTAL_BUF_SIZE] = "...";
  char* beg = buf + 3;

  for (R_len_t i = 0; i < n; ++i) {
    int written = snprintf(beg, FREE_BUF_SIZE, "%d", i + 1);

    if (written >= FREE_BUF_SIZE) {
      Rf_errorcall(R_NilValue, "Can't write repaired names as there are too many.");
    }

    SET_STRING_ELT(nms, i, Rf_mkChar(buf));
  }

  UNPROTECT(1);
  return nms;
}

#undef TOTAL_BUF_SIZE
#undef FREE_BUF_SIZE
