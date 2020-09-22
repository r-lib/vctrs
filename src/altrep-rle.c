#include "vctrs.h"
#include "altrep-rle.h"
#include "altrep.h"

#if (!HAS_ALTREP)

#include <R_ext/Rdynload.h>

void vctrs_init_altrep_rle(DllInfo* dll) { }

SEXP altrep_rle_Make(SEXP input) {
  Rf_error("Need R 3.5+ for Altrep support.");
  return R_NilValue;
}

#else


// Initialised at load time
R_altrep_class_t altrep_rle_class;


SEXP altrep_rle_Make(SEXP input) {
  SEXP res = R_new_altrep(altrep_rle_class, input, R_NilValue);
  MARK_NOT_MUTABLE(res);
  return res;
}

// ALTREP methods -------------------
// The length of the object
inline R_xlen_t altrep_rle_Length(SEXP vec) {
  SEXP data2 = R_altrep_data2(vec);
  if (data2 != R_NilValue) {
    return Rf_xlength(data2);
  }

  R_xlen_t sz = 0;
  SEXP rle = R_altrep_data1(vec);
  int* rle_p = INTEGER(rle);

  for (R_xlen_t i = 0; i < Rf_xlength(rle); ++i) {
    sz += rle_p[i];
  }

  return sz;
}

// What gets printed when .Internal(inspect()) is used
Rboolean altrep_rle_Inspect(SEXP x,
                            int pre,
                            int deep,
                            int pvec,
                            void (*inspect_subtree)(SEXP, int, int, int)) {
  Rprintf("vctrs_altrep_rle (len=%d, materialized=%s)\n",
          altrep_rle_Length(x),
          R_altrep_data2(x) != R_NilValue ? "T" : "F");
  return TRUE;
}

// ALTSTRING methods -----------------

// the element at the index `i`
SEXP altrep_rle_string_Elt(SEXP vec, R_xlen_t i) {
  SEXP data2 = R_altrep_data2(vec);
  if (data2 != R_NilValue) {
    return STRING_ELT(data2, i);
  }

  SEXP rle = R_altrep_data1(vec);
  int* rle_p = INTEGER(rle);
  SEXP nms = Rf_getAttrib(rle, Rf_install("names"));

  R_xlen_t idx = 0;
  while (i >= 0 && idx < Rf_xlength(rle)) {
    i -= rle_p[idx++];
  }

  return STRING_ELT(nms, idx - 1);
}

R_xlen_t find_rle_index(int* rle_data, R_xlen_t i, R_xlen_t size) {
  R_xlen_t idx = 0;
  while (i >= 0 && idx < size) {
    i -= rle_data[idx++];
  }
  return idx - 1;
}


// This is a simple implementation, a more complex one would produce a
// altrep_rle object as well
SEXP altrep_rle_Extract_subset(SEXP x, SEXP indx, SEXP call) {
  SEXP data2 = R_altrep_data2(x);
  // If the vector is already materialized, just fall back to the default
  // implementation
  if (data2 != R_NilValue) {
    return NULL;
  }

  SEXP data1 = R_altrep_data1(x);

  int* index_data = INTEGER(indx);
  R_xlen_t index_n = Rf_length(indx);

  int* rle_data = INTEGER(data1);
  R_xlen_t rle_n = Rf_length(data1);

  SEXP nms = PROTECT(Rf_getAttrib(data1, Rf_install("names")));
  SEXP out = PROTECT(Rf_allocVector(STRSXP, index_n));

  for (R_len_t i = 0; i < index_n; ++i) {
    int index_elt = index_data[i];

    if (index_elt == NA_INTEGER) {
      SET_STRING_ELT(out, i, NA_STRING);
      continue;
    }

    --index_elt;

    R_xlen_t rle_idx = find_rle_index(rle_data, index_elt, rle_n);
    SET_STRING_ELT(out, i, STRING_ELT(nms, rle_idx));
  }

  UNPROTECT(2);
  return out;
}

// --- Altvec
SEXP altrep_rle_string_Materialize(SEXP vec) {
  SEXP data2 = R_altrep_data2(vec);
  if (data2 != R_NilValue) {
    return data2;
  }

  R_xlen_t sz = altrep_rle_Length(vec);
  SEXP rle = R_altrep_data1(vec);
  int* rle_p = INTEGER(rle);

  SEXP out = PROTECT(Rf_allocVector(STRSXP, sz));

  R_xlen_t idx = 0;
  SEXP nms = Rf_getAttrib(rle, Rf_install("names"));
  for (R_xlen_t i = 0; i < Rf_xlength(rle); ++i) {
    for (R_xlen_t j = 0; j < rle_p[i]; ++j) {
      SET_STRING_ELT(out, idx++, STRING_ELT(nms, i));
    }
  }

  UNPROTECT(1);
  R_set_altrep_data2(vec, out);

  return out;
}

void* altrep_rle_Dataptr(SEXP vec, Rboolean writeable) {
  return STDVEC_DATAPTR(altrep_rle_string_Materialize(vec));
}

const void* altrep_rle_Dataptr_or_null(SEXP vec) {
  SEXP data2 = R_altrep_data2(vec);
  if (data2 == R_NilValue)
    return NULL;

  return STDVEC_DATAPTR(data2);
}


void vctrs_init_altrep_rle(DllInfo* dll) {
  altrep_rle_class = R_make_altstring_class("altrep_rle", "vctrs", dll);

  // altrep
  R_set_altrep_Length_method(altrep_rle_class, altrep_rle_Length);
  R_set_altrep_Inspect_method(altrep_rle_class, altrep_rle_Inspect);

  // altvec
  R_set_altvec_Dataptr_method(altrep_rle_class, altrep_rle_Dataptr);
  R_set_altvec_Dataptr_or_null_method(altrep_rle_class, altrep_rle_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(altrep_rle_class, altrep_rle_Extract_subset);

  // altstring
  R_set_altstring_Elt_method(altrep_rle_class, altrep_rle_string_Elt);
}

#endif // R version >= 3.5.0
