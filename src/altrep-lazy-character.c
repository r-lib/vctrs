#include "vctrs.h"
#include "altrep.h"

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
  if (writeable) {
    r_stop_internal("Can't get writeable `DATAPTR()` to `<altrep_lazy_character>`");
  } else {
    // R promises not to write to this array, but we still have to return a
    // `void*` pointer rather than a `const void*` pointer. `STRING_PTR()` is
    // non-API so we use `STRING_PTR_RO()` and cast. This is really a bad ALTREP
    // API. It should have been separated into `void* Dataptr()` and `const
    // void* Dataptr_ro()`.
    return (void*) STRING_PTR_RO(altrep_lazy_character_Materialize(vec));
  }
}

const void* altrep_lazy_character_Dataptr_or_null(r_obj* vec) {
  r_obj* out = R_altrep_data2(vec);

  if (out == r_null) {
    return NULL;
  } else {
    return r_chr_cbegin(out);
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
