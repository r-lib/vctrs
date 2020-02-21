#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"

// -----------------------------------------------------------------------------
#if HAS_ALTREP

SEXP vctrs_is_altrep_vctrs_compact_rep_compact(SEXP x) {
  if (!vec_is_altrep_vctrs_compact_rep(x)) {
    Rf_errorcall(R_NilValue, "Internal error: `x` is not an ALTREP compact rep");
  }

  bool out = VCTRS_COMPACT_REP_IS_COMPACT(x);

  return Rf_ScalarLogical(out);
}

// -----------------------------------------------------------------------------
#else

SEXP vctrs_is_altrep_vctrs_compact_rep_compact(SEXP x) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}

#endif
