#include "vctrs.h"
#include "utils.h"

static SEXP vec_rbind(SEXP xs, SEXP ptype);
static SEXP as_df_row(SEXP x, bool quiet);


// [[ register(external = TRUE) ]]
SEXP vctrs_rbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env));

  SEXP out = vec_rbind(xs, ptype);

  UNPROTECT(2);
  return out;
}


// From type.c
SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);

static SEXP vec_rbind(SEXP xs, SEXP ptype) {
  R_len_t n = Rf_length(xs);

  for (R_len_t i = 0; i < n; ++i) {
    SET_VECTOR_ELT(xs, i, as_df_row(VECTOR_ELT(xs, i), false));
  }

  // The common type holds information about common column names,
  // types, etc. Each element of `xs` needs to be cast to that type
  // before assignment.
  ptype = PROTECT(vctrs_type_common_impl(xs, ptype));

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return new_data_frame(vctrs_shared_empty_list, 0);
  }
  if (!is_data_frame(ptype)) {
    Rf_errorcall(R_NilValue, "Can't bind objects that are not coercible to a data frame.");
  }

  // Find individual input sizes and total size of output
  R_len_t nrow = 0;

  SEXP ns_placeholder = PROTECT(Rf_allocVector(INTSXP, n));
  int* ns = INTEGER(ns_placeholder);

  for (R_len_t i = 0; i < n; ++i) {
    R_len_t size = vec_size(VECTOR_ELT(xs, i));
    nrow += size;
    ns[i] = size;
  }

  SEXP out = PROTECT(vec_na(ptype, nrow));
  SEXP idx = PROTECT(compact_seq(0, 0));
  int* idx_ptr = INTEGER(idx);

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    R_len_t size = ns[i];
    if (!size) {
      continue;
    }

    SEXP tbl = PROTECT(vec_cast(VECTOR_ELT(xs, i), ptype));
    idx_ptr[0] = counter;
    idx_ptr[1] = counter + size;

    df_assign(out, idx, tbl, false);

    counter += size;
    UNPROTECT(1);
  }

  UNPROTECT(4);
  return out;
}


static SEXP as_df_row(SEXP x, bool quiet) {
  if (x == R_NilValue) {
    return x;
  }
  if (vec_is_unspecified(x) && r_names(x) == R_NilValue) {
    return x;
  }
  if (is_data_frame(x)) {
    return x;
  }

  if (vec_dim(x) != 1) {
    return r_as_data_frame(x);
  }

  x = PROTECT(r_as_list(x));

  SEXP nms = PROTECT(vec_unique_names(x, quiet));
  Rf_setAttrib(x, R_NamesSymbol, nms);

  x = new_data_frame(x, 1);

  UNPROTECT(2);
  return x;
}

// [[ register() ]]
SEXP vctrs_as_df_row(SEXP x, SEXP quiet) {
  return as_df_row(x, LOGICAL(quiet)[0]);
}
