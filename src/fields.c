#include <rlang.h>
#include "vctrs.h"

// SEXP x and y must be CHARSXP
// x_utf* is pointer to const char* which is lazily initialised:
//  This makes this function also suitable for use when repeated
//  comparing varying y to constant x
bool equal_string(SEXP x, const char** x_utf8, SEXP y) {
  // Try fast pointer comparison
  if (x == y)
    return true;

  if (*x_utf8 == NULL)
    *x_utf8 = Rf_translateCharUTF8(x);

  // Try slower conversion to common encoding
  const char* y_utf = Rf_translateCharUTF8(y);
  return (strcmp(y_utf, *x_utf8) == 0);
}

int find_offset(SEXP x, SEXP index) {
  if (Rf_length(index) != 1) {
    Rf_errorcall(R_NilValue, "Invalid index: must have length 1");
  }

  int n = Rf_length(x);

  if (TYPEOF(index) == INTSXP) {
    int val = INTEGER(index)[0];

    if (val == NA_INTEGER)
      Rf_errorcall(R_NilValue, "Invalid index: NA_integer_");

    val--;
    if (val < 0 || val >= n)
      Rf_errorcall(R_NilValue, "Invalid index: out of bounds");

    return val;
  } else if (TYPEOF(index) == REALSXP) {
    double val = REAL(index)[0];

    if (R_IsNA(val))
      Rf_errorcall(R_NilValue, "Invalid index: NA_real_");

    val--;
    if (val < 0 || val >= n)
      Rf_errorcall(R_NilValue, "Invalid index: out of bounds");

    if (val > R_LEN_T_MAX) {
      Rf_errorcall(R_NilValue, "Invalid index: too large");
    }

    return (int) val;
  } else if (TYPEOF(index) == STRSXP) {
    SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
    if (names == R_NilValue)
      Rf_errorcall(R_NilValue, "Corrupt x: no names");

    SEXP val_0 = STRING_ELT(index, 0);
    if (val_0 == NA_STRING)
      Rf_errorcall(R_NilValue, "Invalid index: NA_character_");

    const char* val_0_chr = Rf_translateCharUTF8(val_0);
    if (val_0_chr[0] == '\0')
      Rf_errorcall(R_NilValue, "Invalid index: empty string");

    for (int j = 0; j < Rf_length(names); ++j) {
      SEXP name_j = STRING_ELT(names, j);
      if (name_j == NA_STRING)
        Rf_errorcall(R_NilValue, "Corrupt x: element %i is unnamed", j + 1);

      if (equal_string(val_0, &val_0_chr, name_j)) {
        UNPROTECT(1);
        return j;
      }
    }
    Rf_errorcall(R_NilValue, "Invalid index: field name '%s' not found", val_0_chr);
  } else {
    Rf_errorcall(R_NilValue, "Invalid index: must be a character or numeric vector");
  }
}

// Lists -------------------------------------------------------------------

SEXP vctrs_list_get(SEXP x, SEXP index) {
  int idx = find_offset(x, index);

  return VECTOR_ELT(x, idx);
}

SEXP vctrs_list_set(SEXP x, SEXP index, SEXP value) {
  int idx = find_offset(x, index);

  SEXP out = PROTECT(Rf_shallow_duplicate(x));
  SET_VECTOR_ELT(out, idx, value);
  UNPROTECT(1);

  return out;
}

// Records ------------------------------------------------------------------

void check_rcrd(SEXP x) {
  if (!Rf_isVectorList(x))
    Rf_errorcall(R_NilValue, "Corrupt rcrd: not a list");
  if (Rf_length(x) == 0)
    Rf_errorcall(R_NilValue, "Corrupt rcrd: length 0");
}

SEXP vctrs_fields(SEXP x) {
  check_rcrd(x);

  return Rf_getAttrib(x, R_NamesSymbol);
}

SEXP vctrs_n_fields(SEXP x) {
  check_rcrd(x);

  return Rf_ScalarInteger(Rf_length(x));
}

SEXP vctrs_field_get(SEXP x, SEXP index) {
  check_rcrd(x);
  return vctrs_list_get(x, index);
}

SEXP vctrs_field_set(SEXP x, SEXP index, SEXP value) {
  check_rcrd(x);

  if (!vec_is_vector(value)) {
    Rf_errorcall(R_NilValue, "Invalid value: not a vector.");
  }

  if (vec_size(value) != vec_size(x)) {
    Rf_errorcall(R_NilValue, "Invalid value: incorrect length.");
  }

  return vctrs_list_set(x, index, value);
}
