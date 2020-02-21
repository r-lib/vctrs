#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"

// -----------------------------------------------------------------------------
#if HAS_ALTREP

// [[ include("altrep-rep.h") ]]
bool vec_is_altrep_vctrs_compact_rep(SEXP x) {
  SEXP cls = ALTREP_CLASS(x);

  bool out =
    cls == altrep_vctrs_compact_rep_int_class_sexp ||
    cls == altrep_vctrs_compact_rep_dbl_class_sexp ||
    cls == altrep_vctrs_compact_rep_chr_class_sexp;

#if HAS_ALTREP_3_6
  out = out ||
    cls == altrep_vctrs_compact_rep_lgl_class_sexp;
#endif

  return out;
}

// [[ init() ]]
void vctrs_init_altrep_vctrs_compact_rep(DllInfo* dll) {
  vctrs_init_altrep_vctrs_compact_rep_int(dll);
  vctrs_init_altrep_vctrs_compact_rep_dbl(dll);
  vctrs_init_altrep_vctrs_compact_rep_chr(dll);

#if HAS_ALTREP_3_6
  vctrs_init_altrep_vctrs_compact_rep_lgl(dll);
#endif
}

// -----------------------------------------------------------------------------
#else

// [[ include("altrep-rep.h") ]]
bool vec_is_altrep_vctrs_compact_rep(SEXP x) { return false; }

// For DllInfo on R < 3.4
#include <R_ext/Rdynload.h>

// [[ init() ]]
void vctrs_init_altrep_vctrs_compact_rep(DllInfo* dll) { }

#endif
