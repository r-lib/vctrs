#include "unspecified.h"
#include "vctrs.h"

#include "decl/unspecified-decl.h"

// Initialised at load time
static SEXP unspecified_attrib = NULL;
SEXP vctrs_shared_empty_uns = NULL;

static r_obj* syms_vec_ptype_finalise_dispatch = NULL;
static r_obj* fns_vec_ptype_finalise_dispatch = NULL;

// [[ include("vctrs.h") ]]
SEXP vec_unspecified(R_len_t n) {
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  r_lgl_fill(out, NA_LOGICAL, n);
  SET_ATTRIB(out, unspecified_attrib);
  SET_OBJECT(out, 1);

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
  if (TYPEOF(x) != LGLSXP) {
    return false;
  }

  SEXP attrib = ATTRIB(x);

  if (attrib == unspecified_attrib) {
    return true;
  }

  if (attrib != R_NilValue) {
    // The unspecified vector might have been created outside the
    // session (e.g. serialisation)
    if (Rf_inherits(x, "vctrs_unspecified")) {
      return true;
    }
    if (r_is_object(x)) {
      return false;
    }
    if (has_dim(x)) {
      return false;
    }
  }

  R_len_t n = Rf_length(x);
  if (n == 0) {
    return false;
  }

  R_len_t* p_x = LOGICAL(x);
  for (R_len_t i = 0; i < n; ++i) {
    if (p_x[i] != NA_LOGICAL) {
      return false;
    }
  }

  return true;
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

  // TODO!: Should act more like `df_ptype()` for the bare cases, and
  // `s3_ptype()` for the classed df case (proxy/vec_ptype_finalise/restore).
  // Then remove `bare_df_map()` and `df_map()` and unexpose
  // `vec_bare_df_restore()` and `vec_df_restore()`.
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
    SEXP unspecified_class = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(unspecified_class, 0, Rf_mkChar("vctrs_unspecified"));

    unspecified_attrib = Rf_cons(unspecified_class, R_NilValue);
    R_PreserveObject(unspecified_attrib);
    SET_TAG(unspecified_attrib, R_ClassSymbol);

    UNPROTECT(1);
  }

  vctrs_shared_empty_uns = vec_unspecified(0);
  R_PreserveObject(vctrs_shared_empty_uns);
  MARK_NOT_MUTABLE(vctrs_shared_empty_uns);

  syms_vec_ptype_finalise_dispatch = r_sym("vec_ptype_finalise_dispatch");
  fns_vec_ptype_finalise_dispatch = r_eval(syms_vec_ptype_finalise_dispatch, ns);
}
