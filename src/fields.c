#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

int find_offset(SEXP x, SEXP index) {
  if (Rf_length(index) > 1) {
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
  } else if (TYPEOF(index) == STRSXP) {
    SEXP names = Rf_getAttrib(x, R_NamesSymbol);
    if (names == R_NilValue)
      Rf_errorcall(R_NilValue, "Corrupt record: no names");

    if (STRING_ELT(index, 0) == NA_STRING)
      Rf_errorcall(R_NilValue, "Invalid index: NA_character_");

    const char* val = Rf_translateCharUTF8(STRING_ELT(index, 0));
    if (val[0] == '\0')
      Rf_errorcall(R_NilValue, "Invalid index: empty string");

    for (int j = 0; j < Rf_length(names); ++j) {
      if (STRING_ELT(names, j) == NA_STRING)
        Rf_errorcall(R_NilValue, "Corrupt record: element %i is unnamed", j + 1);

      const char* names_j = Rf_translateCharUTF8(STRING_ELT(names, j));
      if (strcmp(names_j, val) == 0)
        return j;
    }
    Rf_errorcall(R_NilValue, "Invalid index: field name '%s' not found", val);
  } else {
    Rf_errorcall(R_NilValue, "Invalid index: must be a character or numeric vector");
  }
}

void check_record(SEXP x) {
  if (!Rf_isVectorList(x))
    Rf_errorcall(R_NilValue, "Corrupt record: not a list");
  if (Rf_length(x) == 0)
    Rf_errorcall(R_NilValue, "Corrupt record: length 0");
}

SEXP vctrs_fields(SEXP x) {
  check_record(x);

  return Rf_getAttrib(x, R_NamesSymbol);
}

SEXP vctrs_n_fields(SEXP x) {
  check_record(x);

  return Rf_ScalarInteger(Rf_length(x));
}

SEXP vctrs_field_get(SEXP x, SEXP index) {
  check_record(x);

  int idx = find_offset(x, index);
  return VECTOR_ELT(x, idx);
}

SEXP vctrs_field_set(SEXP x, SEXP index, SEXP value) {
  check_record(x);

  if (!Rf_isVector(value))
    Rf_errorcall(R_NilValue, "Invalid value: not a vector");
  if (Rf_length(value) != Rf_length(VECTOR_ELT(x, 0)))
    Rf_errorcall(R_NilValue, "Invalid value: incorrect length");

  int idx = find_offset(x, index);
  SEXP out = PROTECT(Rf_shallow_duplicate(x));
  SET_VECTOR_ELT(out, idx, value);
  UNPROTECT(1);

  return out;
}
