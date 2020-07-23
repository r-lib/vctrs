#include "vctrs.h"
#include "type-data-frame.h"
#include "owned.h"
#include "utils.h"

// Initialised at load time
static SEXP syms_vec_restore_dispatch = NULL;
static SEXP fns_vec_restore_dispatch = NULL;

// [[ register() ]]
SEXP vctrs_restore_default(SEXP x, SEXP to) {
  return vec_restore_default(x, to, vec_owned(x));
}

// Copy attributes except names and dim. This duplicates `x` if needed.
// [[ include("vctrs.h") ]]
SEXP vec_restore_default(SEXP x, SEXP to, const enum vctrs_owned owned) {
  SEXP attrib = ATTRIB(to);

  const bool is_s4 = IS_S4_OBJECT(to);

  if (attrib == R_NilValue && !is_s4) {
    return x;
  }

  int n_protect = 0;

  attrib = PROTECT(Rf_shallow_duplicate(attrib));
  ++n_protect;

  x = PROTECT(vec_clone_referenced(x, owned));
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

static SEXP vec_bare_df_restore_impl(SEXP x, SEXP to, R_len_t size,
                                     const enum vctrs_owned owned) {
  x = PROTECT(vec_restore_default(x, to, owned));

  if (Rf_getAttrib(x, R_NamesSymbol) == R_NilValue) {
    SEXP names = PROTECT(Rf_allocVector(STRSXP, Rf_length(x)));
    Rf_setAttrib(x, R_NamesSymbol, names);
    UNPROTECT(1);
  }

  SEXP rownames = PROTECT(df_rownames(x));
  if (rownames == R_NilValue) {
    init_compact_rownames(x, size);
  }

  UNPROTECT(2);
  return x;
}

// [[ register() ]]
SEXP vctrs_bare_df_restore(SEXP x, SEXP to, SEXP n) {
  return vec_bare_df_restore(x, to, n, vec_owned(x));
}

// [[ include("vctrs.h") ]]
SEXP vec_bare_df_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned) {
  if (TYPEOF(x) != VECSXP) {
    stop_internal("vec_bare_df_restore",
                  "Attempt to restore data frame from a %s.",
                  Rf_type2char(TYPEOF(x)));
  }

  R_len_t size = (n == R_NilValue) ? df_raw_size(x) : r_int_get(n, 0);
  return vec_bare_df_restore_impl(x, to, size, owned);
}

// Restore methods are passed the original atomic type back, so we
// first restore data frames as such before calling the restore
// method, if any
// [[ include("vctrs.h") ]]
SEXP vec_df_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned) {
  SEXP out = PROTECT(vec_bare_df_restore(x, to, n, owned));
  out = vec_restore_dispatch(out, to, n);
  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_restore(SEXP x, SEXP to, SEXP n) {
  return vec_restore(x, to, n, vec_owned(x));
}

// FIXME: Having `owned` as an argument to `vec_restore()` may be
// unnecessary once we have recursive proxy / restore mechanisms.
// It currently helps resolve performance issues in `vec_rbind()`'s usage of
// `df_assign()`, which repeatedly proxies and restores each column,
// causing duplication to occur. Passing `owned` through here allows us to
// call `vec_clone_referenced()`, which won't attempt to clone if we know we
// own the object. See #1151.
// [[ include("vctrs.h") ]]
SEXP vec_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned) {
  switch (class_type(to)) {
  default: return vec_restore_dispatch(x, to, n);
  case vctrs_class_bare_factor:
  case vctrs_class_bare_ordered:
  case vctrs_class_none: return vec_restore_default(x, to, owned);
  case vctrs_class_bare_date: return vec_date_restore(x, to, owned);
  case vctrs_class_bare_posixct: return vec_posixct_restore(x, to, owned);
  case vctrs_class_bare_posixlt: return vec_posixlt_restore(x, to, owned);
  case vctrs_class_bare_data_frame:
  case vctrs_class_bare_tibble: return vec_bare_df_restore(x, to, n, owned);
  case vctrs_class_data_frame: return vec_df_restore(x, to, n, owned);
  }
}


void vctrs_init_proxy_restore(SEXP ns) {
  syms_vec_restore_dispatch = Rf_install("vec_restore_dispatch");
  fns_vec_restore_dispatch = Rf_findVar(syms_vec_restore_dispatch, ns);
}
