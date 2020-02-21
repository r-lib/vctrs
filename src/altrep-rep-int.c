#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"

#if (R_VERSION < R_Version(3, 5, 0))
// -----------------------------------------------------------------------------
// Non-ALTREP support

// [[ include("altrep-rep.h") ]]
SEXP new_altrep_vctrs_compact_rep_int(int value, R_xlen_t size) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}

// [[ register() ]]
SEXP vctrs_new_altrep_vctrs_compact_rep_int(SEXP value, SEXP size) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}

#else
// -----------------------------------------------------------------------------
// ALTREP implementation

// [[ include("altrep-rep.h") ]]
SEXP new_altrep_vctrs_compact_rep_int(int value, R_xlen_t size) {
  double size_ = (double) size;

  SEXP info = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(info, 0, Rf_ScalarInteger(value));
  SET_VECTOR_ELT(info, 1, Rf_ScalarReal(size_));

  SEXP out = R_new_altrep(altrep_vctrs_compact_rep_int_class, info, R_NilValue);

  // Force duplicate on modify
  MARK_NOT_MUTABLE(out);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_new_altrep_vctrs_compact_rep_int(SEXP value, SEXP size) {
  int value_ = INTEGER(value)[0];
  R_xlen_t size_ = REAL(size)[0];

  return new_altrep_vctrs_compact_rep_int(value_, size_);
}

// -----------------------------------------------------------------------------

#define VCTRS_COMPACT_REP_INT_VALUE(info) INTEGER0(VECTOR_ELT(info, 0))[0]
#define VCTRS_COMPACT_REP_INT_SIZE(info) ((R_xlen_t) REAL0(VECTOR_ELT(info, 1))[0])

// Materialize the full vector
static SEXP vctrs_compact_rep_int_materialize(SEXP x) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  int value = VCTRS_COMPACT_REP_INT_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_INT_SIZE(info);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = value;
  }

  UNPROTECT(1);
  return out;
}

static SEXP vctrs_compact_rep_int_Serialized_state(SEXP x) {
  return VCTRS_COMPACT_REP_INFO(x);
}

static SEXP vctrs_compact_rep_int_Unserialize(SEXP cls, SEXP state) {
  SEXP info = state;
  int value = VCTRS_COMPACT_REP_INT_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_INT_SIZE(info);

  return new_altrep_vctrs_compact_rep_int(value, size);
}

// TODO: What if `deep = false`? vroom dttm duplicates the altrep object
// but compact_intseq objects always materialize
static SEXP vctrs_compact_rep_int_Duplicate(SEXP x, Rboolean deep) {
  return vctrs_compact_rep_int_materialize(x);
}

// Drop through to standard coercion methods for now.
// We could coerce from one compact rep type to another.
static SEXP vctrs_compact_rep_int_Coerce(SEXP x, int type) {
  return NULL;
}

static Rboolean vctrs_compact_rep_int_Inspect(SEXP x,
                                              int pre,
                                              int deep,
                                              int pvec,
                                              void (*inspect_subtree)(SEXP, int, int, int)) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  int value = VCTRS_COMPACT_REP_INT_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_INT_SIZE(info);
  const char* state = VCTRS_COMPACT_REP_IS_COMPACT(x) ? "compact" : "expanded";

  Rprintf("vctrs_compact_rep_int (value: %i, size: %i, state: %s)", value, size, state);
  Rprintf("\n");

  return TRUE;
}

static R_xlen_t vctrs_compact_rep_int_Length(SEXP x) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_INT_SIZE(info);
}

static void* vctrs_compact_rep_int_Dataptr(SEXP x, Rboolean writeable) {
  if (VCTRS_COMPACT_REP_IS_COMPACT(x)) {
    VCTRS_COMPACT_REP_SET_DATA(x, vctrs_compact_rep_int_materialize(x));
  }

  return DATAPTR(VCTRS_COMPACT_REP_DATA(x));
}

static const void* vctrs_compact_rep_int_Dataptr_or_null(SEXP x) {
  if (VCTRS_COMPACT_REP_IS_COMPACT(x)) {
    return NULL;
  } else {
    return vctrs_compact_rep_int_Dataptr(x, FALSE);
  }
}

static SEXP vctrs_compact_rep_int_Extract_subset(SEXP x, SEXP indx, SEXP call) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  int value = VCTRS_COMPACT_REP_INT_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_INT_SIZE(info);

  R_xlen_t out_size = Rf_xlength(indx);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, out_size));
  int* p_out = INTEGER(out);

  // indx is 1-based
  int* p_indx = INTEGER(indx);

  for (R_xlen_t i = 0; i < out_size; ++i) {
    int loc = p_indx[i];

    if (loc == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    // Mimic normal R vector. OOB = NA.
    if (loc > size) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    p_out[i] = value;
  }

  UNPROTECT(1);
  return out;
}

// I believe we should expect that *_ELT() methods will never contain
// an `NA` index. I assumed this from how ExtractSubset() works and from
// how compact_intseq_Elt() is implemented
static int vctrs_compact_rep_int_Elt(SEXP x, R_xlen_t i) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_INT_VALUE(info);
}

static R_xlen_t vctrs_compact_rep_int_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int* buf) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  int value = VCTRS_COMPACT_REP_INT_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_INT_SIZE(info);

  R_xlen_t n_capped = size - i > n ? n : size - i;

  for (R_xlen_t k = 0; k < n_capped; ++k) {
    buf[k] = value;
  }

  return n_capped;
}

#undef VCTRS_COMPACT_REP_INT_VALUE
#undef VCTRS_COMPACT_REP_INT_SIZE

// -----------------------------------------------------------------------------

SEXP altrep_vctrs_compact_rep_int_class_sexp = NULL;

void vctrs_init_altrep_vctrs_compact_rep_int(DllInfo* dll) {
  altrep_vctrs_compact_rep_int_class = R_make_altinteger_class("vctrs_compact_rep_int", "vctrs", dll);

  altrep_vctrs_compact_rep_int_class_sexp = R_SEXP(altrep_vctrs_compact_rep_int_class);
  R_PreserveObject(altrep_vctrs_compact_rep_int_class_sexp);

  // ALTREP methods
  R_set_altrep_Serialized_state_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Serialized_state);
  R_set_altrep_Unserialize_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Unserialize);
  R_set_altrep_Duplicate_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Duplicate);
  R_set_altrep_Coerce_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Coerce);
  R_set_altrep_Inspect_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Inspect);
  R_set_altrep_Length_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Length);

  // ALTVEC methods
  R_set_altvec_Dataptr_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Dataptr);
  R_set_altvec_Dataptr_or_null_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Extract_subset);

  // ALTINTEGER methods
  R_set_altinteger_Elt_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Elt);
  R_set_altinteger_Get_region_method(altrep_vctrs_compact_rep_int_class, vctrs_compact_rep_int_Get_region);
}

#endif
