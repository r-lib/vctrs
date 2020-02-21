#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"

#if !HAS_ALTREP
// -----------------------------------------------------------------------------
// Non-ALTREP support

// [[ include("altrep-rep.h") ]]
bool vec_is_altrep_vctrs_compact_rep(SEXP x) { return false; }

// For DllInfo on R < 3.4
#include <R_ext/Rdynload.h>

// [[ init() ]]
void vctrs_init_altrep_vctrs_compact_rep(DllInfo* dll) { }

#else
// -----------------------------------------------------------------------------
// ALTREP implementation

// [[ include("altrep-rep.h") ]]
bool vec_is_altrep_vctrs_compact_rep(SEXP x) {
  SEXP cls = ALTREP_CLASS(x);

  return
    cls == altrep_vctrs_compact_rep_lgl_class_sexp ||
    cls == altrep_vctrs_compact_rep_int_class_sexp ||
    cls == altrep_vctrs_compact_rep_dbl_class_sexp;
}

// [[ init() ]]
void vctrs_init_altrep_vctrs_compact_rep(DllInfo* dll) {
  vctrs_init_altrep_vctrs_compact_rep_lgl(dll);
  vctrs_init_altrep_vctrs_compact_rep_int(dll);
  vctrs_init_altrep_vctrs_compact_rep_dbl(dll);
}

#endif
