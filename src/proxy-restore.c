#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

// Initialised at load time
static SEXP syms_vec_restore_dispatch = NULL;
static SEXP fns_vec_restore_dispatch = NULL;


// Copy attributes except names and dim. This duplicates `x` if needed.
// [[ include("vctrs.h") ]]
SEXP vec_restore_default(SEXP x, SEXP to) {
  SEXP attrib = ATTRIB(to);

  const bool is_s4 = IS_S4_OBJECT(to);

  if (attrib == R_NilValue && !is_s4) {
    return x;
  }

  int n_protect = 0;

  attrib = PROTECT(Rf_shallow_duplicate(attrib));
  ++n_protect;

  x = PROTECT(r_clone_referenced(x));
  ++n_protect;

  // Remove vectorised attributes which might be incongruent after reshaping.
  // Shouldn't matter for GNU R but other R implementations might have checks.
  // Also record class to set it later with `Rf_setAttrib()`. This restores
  // the OBJECT bit and is likely more compatible with other implementations.
  SEXP class = R_NilValue;

  {
    SEXP node = attrib;
    SEXP prev = R_NilValue;

    while (node != R_NilValue) {
      SEXP tag = TAG(node);

      // Skip special attributes
      if (tag == R_NamesSymbol || tag == R_DimSymbol ||
          tag == R_DimNamesSymbol || tag == R_ClassSymbol ||
          tag == R_RowNamesSymbol) {
        if (tag == R_ClassSymbol) {
          class = CAR(node);
        }

        if (prev == R_NilValue) {
          attrib = CDR(attrib);
        } else {
          SETCDR(prev, CDR(node));
        }

        node = CDR(node);
        continue;
      }

      prev = node;
      node = CDR(node);
    }
  }

  // Copy attributes but keep names and dims. Don't restore names for
  // shaped objects since those are generated from dimnames.
  SEXP dim = PROTECT(Rf_getAttrib(x, R_DimSymbol));
  ++n_protect;

  if (dim == R_NilValue) {
    SEXP nms = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

    // Check if `to` is a data frame early. If `x` and `to` point
    // to the same reference, then `SET_ATTRIB()` would alter `to`.
    SEXP rownms = PROTECT(df_rownames(x));
    const bool restore_rownms = rownms != R_NilValue && is_data_frame(to);

    SET_ATTRIB(x, attrib);

    Rf_setAttrib(x, R_NamesSymbol, nms);

    // Don't restore row names if `to` isn't a data frame
    if (restore_rownms) {
      Rf_setAttrib(x, R_RowNamesSymbol, rownms);
    }
    UNPROTECT(2);
  } else {
    SEXP dimnames = PROTECT(Rf_getAttrib(x, R_DimNamesSymbol));

    SET_ATTRIB(x, attrib);

    Rf_setAttrib(x, R_DimSymbol, dim);
    Rf_setAttrib(x, R_DimNamesSymbol, dimnames);
    UNPROTECT(1);
  }

  if (class != R_NilValue) {
    Rf_setAttrib(x, R_ClassSymbol, class);
  }

  if (is_s4) {
    r_mark_s4(x);
  }

  UNPROTECT(n_protect);
  return x;
}

static SEXP vec_restore_dispatch(SEXP x, SEXP to, SEXP n) {
  return vctrs_dispatch3(syms_vec_restore_dispatch, fns_vec_restore_dispatch,
                         syms_x, x,
                         syms_to, to,
                         syms_n, n);
}

static SEXP bare_df_restore_impl(SEXP x, SEXP to, R_len_t size) {
  x = PROTECT(r_clone_referenced(x));
  x = PROTECT(vec_restore_default(x, to));

  if (Rf_getAttrib(x, R_NamesSymbol) == R_NilValue) {
    Rf_setAttrib(x, R_NamesSymbol, vctrs_shared_empty_chr);
  }

  SEXP rownames = PROTECT(df_rownames(x));
  if (rownames == R_NilValue) {
    init_compact_rownames(x, size);
  }

  UNPROTECT(3);
  return x;
}

// [[ include("vctrs.h"); register() ]]
SEXP vec_bare_df_restore(SEXP x, SEXP to, SEXP n) {
  if (TYPEOF(x) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: Attempt to restore data frame from a %s.",
                 Rf_type2char(TYPEOF(x)));
  }

  R_len_t size = (n == R_NilValue) ? df_raw_size(x) : r_int_get(n, 0);
  return bare_df_restore_impl(x, to, size);
}

// Restore methods are passed the original atomic type back, so we
// first restore data frames as such before calling the restore
// method, if any
// [[ include("vctrs.h") ]]
SEXP vec_df_restore(SEXP x, SEXP to, SEXP n) {
  SEXP out = PROTECT(vec_bare_df_restore(x, to, n));
  out = vec_restore_dispatch(out, to, n);
  UNPROTECT(1);
  return out;
}

SEXP vec_restore(SEXP x, SEXP to, SEXP n) {
  switch (class_type(to)) {
  default: return vec_restore_dispatch(x, to, n);
  case vctrs_class_bare_factor:
  case vctrs_class_bare_ordered:
  case vctrs_class_none: return vec_restore_default(x, to);
  case vctrs_class_bare_date: return vec_date_restore(x, to);
  case vctrs_class_bare_posixct: return vec_posixct_restore(x, to);
  case vctrs_class_bare_posixlt: return vec_posixlt_restore(x, to);
  case vctrs_class_bare_data_frame:
  case vctrs_class_bare_tibble: return vec_bare_df_restore(x, to, n);
  case vctrs_class_data_frame: return vec_df_restore(x, to, n);
  }
}


void vctrs_init_proxy_restore(SEXP ns) {
  syms_vec_restore_dispatch = Rf_install("vec_restore_dispatch");
  fns_vec_restore_dispatch = Rf_findVar(syms_vec_restore_dispatch, ns);
}
