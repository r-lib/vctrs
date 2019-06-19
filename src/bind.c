#include "vctrs.h"
#include "utils.h"


static SEXP vec_rbind(SEXP xs, SEXP ptype, SEXP id, enum name_repair_arg name_repair);
static SEXP as_df_row(SEXP x, enum name_repair_arg name_repair, bool quiet);
static SEXP as_df_row_impl(SEXP x, enum name_repair_arg name_repair, bool quiet);
enum name_repair_arg validate_bind_name_repair(SEXP name_repair, bool allow_minimal);

// [[ register(external = TRUE) ]]
SEXP vctrs_rbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP id = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  if (id != R_NilValue) {
    if (!r_is_string(id)) {
      Rf_errorcall(R_NilValue, "`.id` must be `NULL` or a string.");
    }
    id = r_chr_get(id, 0);
  }

  enum name_repair_arg repair_arg = validate_bind_name_repair(name_repair, false);
  SEXP out = vec_rbind(xs, ptype, id, repair_arg);

  UNPROTECT(4);
  return out;
}


// From type.c
SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);

static SEXP vec_rbind(SEXP xs, SEXP ptype, SEXP id, enum name_repair_arg name_repair) {
  R_len_t n = Rf_length(xs);

  for (R_len_t i = 0; i < n; ++i) {
    SET_VECTOR_ELT(xs, i, as_df_row(VECTOR_ELT(xs, i), name_repair, false));
  }

  // The common type holds information about common column names,
  // types, etc. Each element of `xs` needs to be cast to that type
  // before assignment.
  ptype = PROTECT(vctrs_type_common_impl(xs, ptype));

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return new_data_frame(vctrs_shared_empty_list, 0);
  }
  if (TYPEOF(ptype) == LGLSXP && !Rf_length(ptype)) {
    UNPROTECT(1);
    ptype = as_df_row_impl(vctrs_shared_na_lgl, name_repair, false);
    PROTECT(ptype);
  }
  if (!is_data_frame(ptype)) {
    Rf_errorcall(R_NilValue, "Can't bind objects that are not coercible to a data frame.");
  }

  // Find individual input sizes and total size of output
  R_len_t nrow = 0;

  SEXP ns_placeholder = PROTECT(Rf_allocVector(INTSXP, n));
  int* ns = INTEGER(ns_placeholder);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    R_len_t size = (elt == R_NilValue) ? 0 : vec_size(elt);
    nrow += size;
    ns[i] = size;
  }

  SEXP out = PROTECT(vec_na(ptype, nrow));
  SEXP idx = PROTECT(compact_seq(0, 0));
  int* idx_ptr = INTEGER(idx);

  SEXP id_col = R_NilValue;
  SEXP* id_col_p = NULL;
  const SEXP* xs_names_p = NULL;

  int nprot = 0;
  if (id != R_NilValue) {
    SEXP xs_names = PROTECT_N(r_names(xs), &nprot);

    if (!r_is_names(xs_names)) {
      Rf_errorcall(R_NilValue, "Inputs must have names when `.id` is supplied");
    }
    xs_names_p = STRING_PTR_RO(xs_names);

    id_col = PROTECT_N(Rf_allocVector(STRSXP, nrow), &nprot);
    id_col_p = STRING_PTR(id_col);
  }

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    R_len_t size = ns[i];
    if (!size) {
      continue;
    }

    if (id != R_NilValue) {
      SEXP nm = xs_names_p[i];
      for (R_len_t j = 0; j < size; ++j, ++id_col_p) {
        *id_col_p = nm;
      }
    }

    SEXP tbl = PROTECT(vec_cast(VECTOR_ELT(xs, i), ptype, args_empty, args_empty));
    init_compact_seq(idx_ptr, counter, counter + size);
    df_assign(out, idx, tbl, false);

    counter += size;
    UNPROTECT(1);
  }

  if (id != R_NilValue) {
    out = df_poke_at(out, id, id_col);
  }

  UNPROTECT(4 + nprot);
  return out;
}

static SEXP as_df_row(SEXP x, enum name_repair_arg name_repair, bool quiet) {
  if (vec_is_unspecified(x) && r_names(x) == R_NilValue) {
    return x;
  } else {
    return as_df_row_impl(x, name_repair, quiet);
  }
}

static SEXP as_df_row_impl(SEXP x, enum name_repair_arg name_repair, bool quiet) {
  if (x == R_NilValue) {
    return x;
  }
  if (is_data_frame(x)) {
    return x;
  }
  if (vec_dim_n(x) != 1) {
    return r_as_data_frame(x);
  }

  x = PROTECT(r_as_list(x));

  SEXP nms = PROTECT(r_names(x));
  if (nms == R_NilValue) {
    nms = PROTECT(vec_unique_names(x, quiet));
  } else {
    nms = PROTECT(vec_as_names(nms, name_repair, quiet));
  }

  Rf_setAttrib(x, R_NamesSymbol, nms);
  x = new_data_frame(x, 1);

  UNPROTECT(3);
  return x;
}

// [[ register() ]]
SEXP vctrs_as_df_row(SEXP x, SEXP quiet) {
  return as_df_row(x, name_repair_unique, LOGICAL(quiet)[0]);
}

static SEXP as_df_col(SEXP x, SEXP outer);
static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, enum name_repair_arg name_repair);
static SEXP cbind_container_type(SEXP x);

// [[ register(external = TRUE) ]]
SEXP vctrs_cbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP size = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  enum name_repair_arg repair_arg = validate_bind_name_repair(name_repair, true);
  SEXP out = vec_cbind(xs, ptype, size, repair_arg);

  UNPROTECT(4);
  return out;
}

static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, enum name_repair_arg name_repair) {
  R_len_t n = Rf_length(xs);

  // Find the common container type of inputs
  SEXP containers = PROTECT(map(xs, &cbind_container_type));
  ptype = PROTECT(cbind_container_type(ptype));

  SEXP type = PROTECT(vctrs_type_common_impl(containers, ptype));
  if (type == R_NilValue) {
    type = new_data_frame(vctrs_shared_empty_list, 0);
  } else if (!is_data_frame(type)) {
    type = r_as_data_frame(type);
  }
  UNPROTECT(1);
  PROTECT(type);


  R_len_t nrow;
  if (size == R_NilValue) {
    nrow = vec_size_common(xs, 0);
  } else {
    nrow = size_validate(size, ".size");
  }


  // Convert inputs to data frames, validate, and collect total number of columns
  SEXP xs_names = PROTECT(r_names(xs));
  bool has_names = xs_names != R_NilValue;
  SEXP* xs_names_ptr = has_names ? STRING_PTR(xs_names) : NULL;

  R_len_t ncol = 0;
  for (R_len_t i = 0; i < n; ++i, ++xs_names_ptr) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    x = PROTECT(vec_recycle(x, nrow));
    x = PROTECT(as_df_col(x, has_names ? *xs_names_ptr : R_NilValue));

    SET_VECTOR_ELT(xs, i, x);
    UNPROTECT(2);

    ncol += Rf_length(x);
  }


  // Fill in columns
  SEXP out = PROTECT(Rf_allocVector(VECSXP, ncol));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, ncol));

  SEXP idx = PROTECT(compact_seq(0, 0));
  int* idx_ptr = INTEGER(idx);

  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    R_len_t xn = Rf_length(x);
    init_compact_seq(idx_ptr, counter, counter + xn);
    list_assign(out, idx, x, false);

    SEXP xnms = PROTECT(r_names(x));
    if (xnms != R_NilValue) {
      chr_assign(names, idx, xnms, false);
    }

    counter += xn;
    UNPROTECT(1);
  }

  names = PROTECT(vec_as_names(names, name_repair, false));
  Rf_setAttrib(out, R_NamesSymbol, names);

  out = vec_restore(out, type, R_NilValue);

  UNPROTECT(8);
  return out;
}

static SEXP cbind_container_type(SEXP x) {
  if (is_data_frame(x)) {
    return df_container_type(x);
  } else {
    return R_NilValue;
  }
}


static SEXP df_as_df_col(SEXP x, SEXP outer);
static SEXP shaped_as_df_col(SEXP x, SEXP outer);
static SEXP vec_as_df_col(SEXP x, SEXP outer);

// [[ register() ]]
SEXP vctrs_as_df_col(SEXP x, SEXP outer) {
  return as_df_col(x, r_chr_get(outer, 0));
}
static SEXP as_df_col(SEXP x, SEXP outer) {
  if (is_data_frame(x)) {
    return df_as_df_col(x, outer);
  }
  if (vec_dim_n(x) != 1) {
    return shaped_as_df_col(x, outer);
  }
  return vec_as_df_col(x, outer);
}

static SEXP vec_as_df_col(SEXP x, SEXP outer) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(out, 0, x);

  if (outer != R_NilValue) {
    SEXP names = PROTECT(r_str_as_character(outer));
    Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(1);
  }

  init_data_frame(out, Rf_length(x));

  UNPROTECT(1);
  return out;
}

static SEXP df_as_df_col(SEXP x, SEXP outer) {
  x = PROTECT(Rf_shallow_duplicate(x));

  SEXP nms = PROTECT(r_names(x));
  nms = PROTECT(outer_names(nms, outer, Rf_length(x)));
  Rf_setAttrib(x, R_NamesSymbol, nms);

  UNPROTECT(3);
  return x;
}

static SEXP shaped_as_df_col(SEXP x, SEXP outer) {
  SEXP nms = PROTECT(colnames(x));
  x = PROTECT(r_as_data_frame(x));

  nms = PROTECT(outer_names(nms, outer, Rf_length(x)));
  Rf_setAttrib(x, R_NamesSymbol, nms);

  UNPROTECT(3);
  return x;
}

enum name_repair_arg validate_bind_name_repair(SEXP name_repair, bool allow_minimal) {
  enum name_repair_arg arg = validate_name_repair(name_repair);

  switch (arg) {
  case name_repair_unique: break;
  case name_repair_universal: break;
  case name_repair_check_unique: break;
  case name_repair_minimal: if (allow_minimal) break;
  default:
    if (allow_minimal) {
      Rf_errorcall(R_NilValue,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, `\"check_unique\"`, or `\"minimal\"`.",
                   name_repair_arg_as_c_string(arg));
    } else {
      Rf_errorcall(R_NilValue,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, or `\"check_unique\"`.",
                   name_repair_arg_as_c_string(arg));
    }
  }

  return arg;
}
