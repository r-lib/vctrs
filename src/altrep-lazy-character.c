#include "altrep.h"

#if (!HAS_ALTREP)

#include <cstddef>

#include <R_ext/Altrep.h>
#include <R_ext/Boolean.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include "call.h"
#include "cnd.h"
#include "eval.h"
#include "globals.h"
#include "obj.h"
#include "rlang-types.h"
#include "utils.h"

void vctrs_init_altrep_lazy_character(DllInfo* dll) { }

r_obj* ffi_altrep_lazy_character_is_materialized(r_obj* x) {
  r_stop_internal("Need R 3.5+ for Altrep support.");
  return r_null;
}

r_obj* ffi_altrep_new_lazy_character(r_obj* fn) {
  r_stop_internal("Need R 3.5+ for Altrep support.");
  return r_null;
}

#else

// Initialised at load time
R_altrep_class_t altrep_lazy_character_class;

r_obj* ffi_altrep_lazy_character_is_materialized(r_obj* x) {
  return r_lgl(R_altrep_data2(x) != r_null);
}

r_obj* ffi_altrep_new_lazy_character(r_obj* fn) {
  r_obj* out = R_new_altrep(altrep_lazy_character_class, fn, r_null);
  r_mark_shared(out);
  return out;
}

// -----------------------------------------------------------------------------
// ALTVEC

r_obj* altrep_lazy_character_Materialize(r_obj* vec) {
  r_obj* out = R_altrep_data2(vec);
  if (out != r_null) {
    return out;
  }

  r_obj* fn = R_altrep_data1(vec);
  r_obj* call = KEEP(r_new_call(fn, r_null));

  // `fn()` evaluated in the global environment
  out = r_eval(call, r_envs.global);

  if (r_typeof(out) != R_TYPE_character) {
    r_stop_internal("`fn` must evaluate to a character vector.");
  }

  R_set_altrep_data2(vec, out);

  UNPROTECT(1);
  return out;
}

void* altrep_lazy_character_Dataptr(r_obj* vec, Rboolean writeable) {
  return STDVEC_DATAPTR(altrep_lazy_character_Materialize(vec));
}

const void* altrep_lazy_character_Dataptr_or_null(r_obj* vec) {
  r_obj* out = R_altrep_data2(vec);

  if (out == r_null) {
    return NULL;
  } else {
    return STDVEC_DATAPTR(out);
  }
}

// -----------------------------------------------------------------------------
// ALTREP

R_xlen_t altrep_lazy_character_Length(r_obj* vec) {
  r_obj* out = R_altrep_data2(vec);

  if (out == r_null) {
    out = altrep_lazy_character_Materialize(vec);
  }

  return r_length(out);
}

// What gets printed when .Internal(inspect()) is used
Rboolean altrep_lazy_character_Inspect(r_obj* x,
                                       int pre,
                                       int deep,
                                       int pvec,
                                       void (*inspect_subtree)(r_obj*, int, int, int)) {
  Rprintf("vctrs_altrep_lazy_character (materialized=%s)\n",
          R_altrep_data2(x) != r_null ? "T" : "F");
  return TRUE;
}

// -----------------------------------------------------------------------------
// ALTSTRING

r_obj* altrep_lazy_character_Elt(r_obj* vec, R_xlen_t i) {
  r_obj* out = R_altrep_data2(vec);

  if (out == r_null) {
    out = altrep_lazy_character_Materialize(vec);
  }

  return STRING_ELT(out, i);
}

void altrep_lazy_character_Set_elt(r_obj* vec, R_xlen_t i, r_obj* value) {
  r_obj* out = R_altrep_data2(vec);

  if (out == r_null) {
    out = altrep_lazy_character_Materialize(vec);
  }

  SET_STRING_ELT(out, i, value);
}

// -----------------------------------------------------------------------------

void vctrs_init_altrep_lazy_character(DllInfo* dll) {
  altrep_lazy_character_class = R_make_altstring_class("altrep_lazy_character", "vctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(altrep_lazy_character_class, altrep_lazy_character_Dataptr);
  R_set_altvec_Dataptr_or_null_method(altrep_lazy_character_class, altrep_lazy_character_Dataptr_or_null);

  // ALTREP
  R_set_altrep_Length_method(altrep_lazy_character_class, altrep_lazy_character_Length);
  R_set_altrep_Inspect_method(altrep_lazy_character_class, altrep_lazy_character_Inspect);

  // ALTSTRING
  R_set_altstring_Elt_method(altrep_lazy_character_class, altrep_lazy_character_Elt);
  R_set_altstring_Set_elt_method(altrep_lazy_character_class, altrep_lazy_character_Set_elt);
}

#endif // R version >= 3.5.0
