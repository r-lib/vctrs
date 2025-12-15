#include "unspecified.h"
#include "vctrs.h"

#include "decl/unspecified-decl.h"

// Initialised at load time
static SEXP vctrs_unspecified_class = NULL;
SEXP vctrs_shared_empty_uns = NULL;

static r_obj* syms_vec_ptype_finalise_dispatch = NULL;
static r_obj* fns_vec_ptype_finalise_dispatch = NULL;

// [[ include("vctrs.h") ]]
SEXP vec_unspecified(R_len_t n) {
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  r_lgl_fill(out, NA_LOGICAL, n);
  r_attrib_poke_class(out, vctrs_unspecified_class);

  UNPROTECT(1);
  return out;
}

// [[ register ]]
SEXP vctrs_unspecified(SEXP n) {
  if (Rf_length(n) != 1) {
    Rf_errorcall(R_NilValue, "`n` must be a single number");
  }
  if (TYPEOF(n) != INTSXP) {
    n = vec_cast(n,
                 r_globals.empty_int,
                 vec_args.empty,
                 vec_args.empty,
                 r_lazy_null);
  }
  int len = INTEGER(n)[0];
  return vec_unspecified(len);
}

// [[ include("vctrs.h") ]]
bool vec_is_unspecified(SEXP x) {
  if (r_typeof(x) != R_TYPE_logical) {
    return false;
  }

  if (r_attrib(x) == r_null) {
    // Bare logical (no class, no dim)
    const r_ssize size = r_length(x);

    if (size == 0) {
      return false;
    }

    const int* p_x = r_lgl_cbegin(x);

    for (r_ssize i = 0; i < size; ++i) {
      if (p_x[i] != r_globals.na_lgl) {
        return false;
      }
    }

    return true;
  }

  return Rf_inherits(x, "vctrs_unspecified");
}

// [[ register ]]
SEXP vctrs_is_unspecified(SEXP x) {
  return Rf_ScalarLogical(vec_is_unspecified(x));
}

// [[ register() ]]
r_obj* vec_ptype_finalise(r_obj* x) {
  if (x == r_null) {
    return x;
  }

  struct r_lazy call = lazy_calls.vec_ptype_finalise;

  if (!r_is_object(x)) {
    obj_check_vector(x, VCTRS_ALLOW_NULL_no, vec_args.x, call);
    return x;
  }

  if (vec_is_unspecified(x)) {
    return vec_ptype_finalise_unspecified(x);
  }

  obj_check_vector(x, VCTRS_ALLOW_NULL_no, vec_args.x, call);

  switch (class_type(x)) {
  case VCTRS_CLASS_bare_tibble:
  case VCTRS_CLASS_bare_data_frame:
    return bare_df_map(x, &vec_ptype_finalise);

  case VCTRS_CLASS_data_frame:
    return df_map(x, &vec_ptype_finalise);

  case VCTRS_CLASS_none:
    r_stop_internal("Non-S3 classes should have returned by now.");

  default:
    return vec_ptype_finalise_dispatch(x);
  }
}

static
r_obj* vec_ptype_finalise_unspecified(r_obj* x) {
  r_ssize size = r_length(x);

  if (size == 0) {
    return r_globals.empty_lgl;
  }

  r_obj* out = KEEP(r_alloc_logical(size));
  r_lgl_fill(out, r_globals.na_lgl, size);

  FREE(1);
  return out;
}

static
r_obj* vec_ptype_finalise_dispatch(r_obj* x) {
  return vctrs_dispatch1(
    syms_vec_ptype_finalise_dispatch, fns_vec_ptype_finalise_dispatch,
    syms_x, x
  );
}

void vctrs_init_unspecified(SEXP ns) {
  {
    vctrs_unspecified_class = Rf_allocVector(STRSXP, 1);
    R_PreserveObject(vctrs_unspecified_class);
    SET_STRING_ELT(vctrs_unspecified_class, 0, Rf_mkChar("vctrs_unspecified"));
  }

  vctrs_shared_empty_uns = vec_unspecified(0);
  R_PreserveObject(vctrs_shared_empty_uns);
  MARK_NOT_MUTABLE(vctrs_shared_empty_uns);

  syms_vec_ptype_finalise_dispatch = r_sym("vec_ptype_finalise_dispatch");
  fns_vec_ptype_finalise_dispatch = r_eval(syms_vec_ptype_finalise_dispatch, ns);
}
