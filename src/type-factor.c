#include "vctrs.h"

static SEXP levels_union(SEXP x, SEXP y);

// [[ include("type-factor.h") ]]
SEXP fct_ptype2(const struct ptype2_opts* opts) {
  SEXP x = opts->x;
  SEXP y = opts->y;

  SEXP x_levels = Rf_getAttrib(x, R_LevelsSymbol);
  SEXP y_levels = Rf_getAttrib(y, R_LevelsSymbol);

  if (TYPEOF(x_levels) != STRSXP) {
    stop_corrupt_factor_levels(x, opts->p_x_arg);
  }

  if (TYPEOF(y_levels) != STRSXP) {
    stop_corrupt_factor_levels(y, opts->p_y_arg);
  }

  // Quick early exit for identical levels pointing to the same SEXP
  if (x_levels == y_levels) {
    return new_empty_factor(x_levels);
  }

  SEXP levels = PROTECT(levels_union(x_levels, y_levels));

  SEXP out = new_empty_factor(levels);

  UNPROTECT(1);
  return out;
}

static
bool ord_ptype2_validate(r_obj* x_levels,
                         r_obj* y_levels,
                         const struct ptype2_opts* p_opts) {
  if (TYPEOF(x_levels) != STRSXP) {
    stop_corrupt_ordered_levels(p_opts->x, p_opts->p_x_arg);
  }
  if (TYPEOF(y_levels) != STRSXP) {
    stop_corrupt_ordered_levels(p_opts->y, p_opts->p_y_arg);
  }

  return equal_object(x_levels, y_levels);
}

// [[ include("type-factor.h") ]]
r_obj* ord_ptype2(const struct ptype2_opts* p_opts) {
  r_obj* x_levels = r_attrib_get(p_opts->x, R_LevelsSymbol);
  r_obj* y_levels = r_attrib_get(p_opts->y, R_LevelsSymbol);

  if (ord_ptype2_validate(x_levels, y_levels, p_opts)) {
    return new_empty_ordered(x_levels);
  } else {
    return vec_ptype2_default(p_opts->x,
                              p_opts->y,
                              p_opts->p_x_arg,
                              p_opts->p_y_arg,
                              r_lazy_null,
                              &p_opts->fallback);
  }
}

static SEXP levels_union(SEXP x, SEXP y) {
  SEXP args = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(args, 0, x);
  SET_VECTOR_ELT(args, 1, y);

  const struct name_repair_opts name_repair_opts = {
    .type = NAME_REPAIR_none,
    .fn = R_NilValue
  };

  // Combine with known ptype
  // No name repair because this is just combining factor levels
  SEXP xy = PROTECT(vec_c(
    args,
    r_globals.empty_chr,
    R_NilValue,
    &name_repair_opts,
    vec_args.empty,
    r_lazy_null
  ));

  SEXP out = vec_unique(xy);

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

static void init_factor(SEXP x, SEXP levels);
static void init_ordered(SEXP x, SEXP levels);


// [[ include("vctrs.h") ]]
SEXP fct_as_character(SEXP x, struct vctrs_arg* x_arg) {
  SEXP levels = PROTECT(Rf_getAttrib(x, R_LevelsSymbol));

  if (TYPEOF(levels) != STRSXP) {
    stop_corrupt_factor_levels(x, x_arg);
  }

  UNPROTECT(1);
  return Rf_asCharacterFactor(x);
}

// [[ include("vctrs.h") ]]
SEXP ord_as_character(SEXP x, struct vctrs_arg* x_arg) {
  return fct_as_character(x, x_arg);
}


static SEXP chr_as_factor_from_self(SEXP x, bool ordered);
static SEXP chr_as_factor_impl(SEXP x, SEXP levels, bool* lossy, bool ordered);

// [[ include("vctrs.h") ]]
SEXP chr_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg) {
  SEXP levels = PROTECT(Rf_getAttrib(to, R_LevelsSymbol));

  if (TYPEOF(levels) != STRSXP) {
    stop_corrupt_factor_levels(to, to_arg);
  }

  SEXP out;

  // When `to` has no levels, it is treated as a template and the
  // levels come from `x`
  if (vec_size(levels) == 0) {
    out = chr_as_factor_from_self(x, false);
  } else {
    out = chr_as_factor_impl(x, levels, lossy, false);
  }

  UNPROTECT(1);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP chr_as_ordered(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg) {
  SEXP levels = PROTECT(Rf_getAttrib(to, R_LevelsSymbol));

  if (TYPEOF(levels) != STRSXP) {
    stop_corrupt_ordered_levels(to, to_arg);
  }

  SEXP out;

  // When `to` has no levels, it is treated as a template and the
  // levels come from `x`
  if (vec_size(levels) == 0) {
    out = chr_as_factor_from_self(x, true);
  } else {
    out = chr_as_factor_impl(x, levels, lossy, true);
  }

  UNPROTECT(1);
  return out;
}

static SEXP chr_as_factor_impl(SEXP x, SEXP levels, bool* lossy, bool ordered) {
  SEXP out = PROTECT(vec_match(x, levels));
  const int* p_out = INTEGER(out);

  R_len_t size = vec_size(x);
  const SEXP* p_x = STRING_PTR_RO(x);

  // Detect lossy no-matches, but allow `NA` values from `x`
  for (R_len_t i = 0; i < size; ++i) {
    if (p_out[i] == NA_INTEGER && p_x[i] != NA_STRING) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }
  }

  if (ordered) {
    init_ordered(out, levels);
  } else {
    init_factor(out, levels);
  }

  UNPROTECT(1);
  return out;
}

static SEXP remove_na_levels(SEXP levels);

// Factor levels are added in order of appearance.
// `NA` values in `x` are not considered factor levels.
static SEXP chr_as_factor_from_self(SEXP x, bool ordered) {
  SEXP levels = PROTECT(vec_unique(x));
  levels = PROTECT(remove_na_levels(levels));

  // `NA` values in `x` correctly become `NA` values in the result
  SEXP out = PROTECT(vec_match(x, levels));

  if (ordered) {
    init_ordered(out, levels);
  } else {
    init_factor(out, levels);
  }

  UNPROTECT(3);
  return out;
}

static SEXP remove_na_levels(SEXP levels) {
  R_len_t size = vec_size(levels);
  const SEXP* p_levels = STRING_PTR_RO(levels);

  // There might only ever be 1 `NA` level.
  // Remove it if it exists.
  for (R_len_t i = 0; i < size; ++i) {
    if (p_levels[i] == NA_STRING) {
      int na_loc = (i + 1) * -1;

      SEXP na_loc_obj = PROTECT(r_int(na_loc));
      SEXP out = vec_slice(levels, na_loc_obj);

      UNPROTECT(1);
      return out;
    }
  }

  return levels;
}


static SEXP fct_as_factor_impl(SEXP x, SEXP x_levels, SEXP to_levels, bool* lossy, bool ordered);

// [[ include("vctrs.h") ]]
SEXP fct_as_factor(SEXP x,
                   SEXP to,
                   bool* lossy,
                   struct vctrs_arg* x_arg,
                   struct vctrs_arg* to_arg) {

  SEXP x_levels = PROTECT(Rf_getAttrib(x, R_LevelsSymbol));
  SEXP to_levels = PROTECT(Rf_getAttrib(to, R_LevelsSymbol));

  if (TYPEOF(x_levels) != STRSXP) {
    stop_corrupt_factor_levels(x, x_arg);
  }

  if (TYPEOF(to_levels) != STRSXP) {
    stop_corrupt_factor_levels(to, to_arg);
  }

  SEXP out = fct_as_factor_impl(x, x_levels, to_levels, lossy, false);

  UNPROTECT(2);
  return out;
}

// [[ include("factor.h") ]]
SEXP ord_as_ordered(const struct cast_opts* p_opts) {
  r_obj* x_levels = r_attrib_get(p_opts->x, R_LevelsSymbol);
  r_obj* y_levels = r_attrib_get(p_opts->to, R_LevelsSymbol);

  struct ptype2_opts ptype2_opts = cast_opts_as_ptype2_opts(p_opts);

  if (ord_ptype2_validate(x_levels, y_levels, &ptype2_opts)) {
    return p_opts->x;
  } else {
    return vec_cast_default(p_opts->x,
                            p_opts->to,
                            p_opts->p_x_arg,
                            p_opts->p_to_arg,
                            p_opts->call,
                            &p_opts->fallback);
  }
}

static SEXP fct_as_factor_impl(SEXP x, SEXP x_levels, SEXP to_levels, bool* lossy, bool ordered) {
  // Early exit if levels are identical
  if (x_levels == to_levels) {
    return x;
  }

  R_len_t x_levels_size = vec_size(x_levels);
  R_len_t to_levels_size = vec_size(to_levels);

  // Early exit if `to` has no levels. In this case it is being used as
  // a template
  if (to_levels_size == 0) {
    return x;
  }

  // Always lossy if there are more levels in `x` than in `to`
  if (x_levels_size > to_levels_size) {
    *lossy = true;
    return R_NilValue;
  }

  R_len_t x_size = vec_size(x);

  const SEXP* p_x_levels = STRING_PTR_RO(x_levels);
  const SEXP* p_to_levels = STRING_PTR_RO(to_levels);

  bool is_contiguous_subset = true;

  for (R_len_t i = 0; i < x_levels_size; ++i) {
    if (p_x_levels[i] != p_to_levels[i]) {
      is_contiguous_subset = false;
      break;
    }
  }

  // No recoding required if contiguous subset.
  // Duplicate, strip non-factor attributes, and re-initialize with new levels.
  // Using `r_clone_referenced()` avoids an immediate copy using ALTREP wrappers.
  if (is_contiguous_subset) {
    SEXP out = PROTECT(r_clone_referenced(x));
    SET_ATTRIB(out, R_NilValue);

    if (ordered) {
      init_ordered(out, to_levels);
    } else {
      init_factor(out, to_levels);
    }

    UNPROTECT(1);
    return out;
  }

  const int* p_x = INTEGER_RO(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, x_size));
  int* p_out = INTEGER(out);

  if (ordered) {
    init_ordered(out, to_levels);
  } else {
    init_factor(out, to_levels);
  }

  SEXP recode = PROTECT(vec_match(x_levels, to_levels));
  const int* p_recode = INTEGER_RO(recode);

  // Detect if there are any levels in `x` that aren't in `to`
  for (R_len_t i = 0; i < x_levels_size; ++i) {
    if (p_recode[i] == NA_INTEGER) {
      *lossy = true;
      UNPROTECT(2);
      return R_NilValue;
    }
  }

  // Recode `x` int values into `to` level ordering
  for (R_len_t i = 0; i < x_size; ++i) {
    const int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    p_out[i] = p_recode[elt - 1];
  }

  UNPROTECT(2);
  return out;
}


static void init_factor(SEXP x, SEXP levels) {
  if (TYPEOF(x) != INTSXP) {
    r_stop_internal("Only integers can be made into factors.");
  }

  Rf_setAttrib(x, R_LevelsSymbol, levels);
  Rf_setAttrib(x, R_ClassSymbol, classes_factor);
}

static void init_ordered(SEXP x, SEXP levels) {
  if (TYPEOF(x) != INTSXP) {
    r_stop_internal("Only integers can be made into ordered factors.");
  }

  Rf_setAttrib(x, R_LevelsSymbol, levels);
  Rf_setAttrib(x, R_ClassSymbol, classes_ordered);
}
