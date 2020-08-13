#include "vctrs.h"
#include "c.h"
#include "dim.h"
#include "ptype-common.h"
#include "slice-assign.h"
#include "type-data-frame.h"
#include "owned.h"
#include "utils.h"


static SEXP vec_rbind(SEXP xs, SEXP ptype, SEXP id, struct name_repair_opts* name_repair, SEXP name_spec);
static SEXP as_df_row(SEXP x, struct name_repair_opts* name_repair);
static SEXP as_df_row_impl(SEXP x, struct name_repair_opts* name_repair);
struct name_repair_opts validate_bind_name_repair(SEXP name_repair, bool allow_minimal);
static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, struct name_repair_opts* name_repair);
static SEXP cbind_names_to(bool has_names, SEXP names_to, SEXP ptype);

// [[ register(external = TRUE) ]]
SEXP vctrs_rbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP names_to = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_spec = PROTECT(Rf_eval(CAR(args), env));

  if (names_to != R_NilValue) {
    if (Rf_inherits(names_to, "rlang_zap")) {
      r_poke_names(xs, R_NilValue);
      names_to = R_NilValue;
    } else if (r_is_string(names_to)) {
      names_to = r_chr_get(names_to, 0);
    } else {
      Rf_errorcall(R_NilValue, "`.names_to` must be `NULL`, a string, or an `rlang::zap()` object.");
    }
  }

  struct name_repair_opts name_repair_opts = validate_bind_name_repair(name_repair, false);
  PROTECT_NAME_REPAIR_OPTS(&name_repair_opts);

  SEXP out = vec_rbind(xs, ptype, names_to, &name_repair_opts, name_spec);

  UNPROTECT(6);
  return out;
}

static SEXP vec_rbind(SEXP xs,
                      SEXP ptype,
                      SEXP names_to,
                      struct name_repair_opts* name_repair,
                      SEXP name_spec) {
  int n_prot = 0;
  R_len_t n_inputs = Rf_length(xs);

  for (R_len_t i = 0; i < n_inputs; ++i) {
    SET_VECTOR_ELT(xs, i, as_df_row(VECTOR_ELT(xs, i), name_repair));
  }

  // The common type holds information about common column names,
  // types, etc. Each element of `xs` needs to be cast to that type
  // before assignment.
  ptype = vec_ptype_common_params(xs, ptype, DF_FALLBACK_DEFAULT, S3_FALLBACK_true);
  PROTECT_N(ptype, &n_prot);

  R_len_t n_cols = Rf_length(ptype);

  if (ptype == R_NilValue) {
    UNPROTECT(n_prot);
    return new_data_frame(vctrs_shared_empty_list, 0);
  }
  if (TYPEOF(ptype) == LGLSXP && !n_cols) {
    ptype = as_df_row_impl(vctrs_shared_na_lgl, name_repair);
    PROTECT_N(ptype, &n_prot);
  }
  if (!is_data_frame(ptype)) {
    Rf_errorcall(R_NilValue, "Can't bind objects that are not coercible to a data frame.");
  }

  SEXP nms = PROTECT_N(r_names(xs), &n_prot);
  bool has_names = nms != R_NilValue;
  bool has_names_to = names_to != R_NilValue;
  R_len_t names_to_loc = 0;

  if (has_names_to) {
    SEXP ptype_nms = PROTECT(r_names(ptype));
    names_to_loc = r_chr_find(ptype_nms, names_to);
    UNPROTECT(1);

    if (names_to_loc < 0) {
      ptype = PROTECT_N(cbind_names_to(has_names, names_to, ptype), &n_prot);
      names_to_loc = 0;
    }
  }

  // Must happen after the `names_to` column has been added to `ptype`
  xs = vec_cast_common_params(xs, ptype, DF_FALLBACK_DEFAULT, S3_FALLBACK_true);
  PROTECT_N(xs, &n_prot);

  // Find individual input sizes and total size of output
  R_len_t n_rows = 0;

  bool has_rownames = false;
  if (!has_names_to && r_names(xs) != R_NilValue) {
    // Names of inputs become row names when `names_to` isn't supplied
    has_rownames = true;
  }

  SEXP ns_placeholder = PROTECT_N(Rf_allocVector(INTSXP, n_inputs), &n_prot);
  int* ns = INTEGER(ns_placeholder);

  for (R_len_t i = 0; i < n_inputs; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    R_len_t size = (elt == R_NilValue) ? 0 : vec_size(elt);
    n_rows += size;
    ns[i] = size;

    if (!has_rownames && is_data_frame(elt)) {
      has_rownames = rownames_type(df_rownames(elt)) == ROWNAMES_IDENTIFIERS;
    }
  }

  SEXP proxy = PROTECT(vec_proxy(ptype));
  if (!is_data_frame(proxy)) {
    Rf_errorcall(R_NilValue, "Can't fill a data frame that doesn't have a data frame proxy.");
  }

  PROTECT_INDEX out_pi;
  SEXP out = vec_init(proxy, n_rows);
  PROTECT_WITH_INDEX(out, &out_pi);

  SEXP idx = PROTECT_N(compact_seq(0, 0, true), &n_prot);
  int* idx_ptr = INTEGER(idx);

  PROTECT_INDEX rownames_pi;
  SEXP rownames = R_NilValue;
  if (has_rownames) {
    rownames = Rf_allocVector(STRSXP, n_rows);
  }
  PROTECT_WITH_INDEX(rownames, &rownames_pi);

  const SEXP* nms_p = NULL;
  if (has_names) {
    nms_p = STRING_PTR_RO(nms);
  }

  SEXP names_to_col = R_NilValue;
  SEXPTYPE names_to_type = 99;
  void* p_names_to_col = NULL;
  const void* p_index = NULL;

  if (has_names_to) {
    SEXP index = R_NilValue;
    if (has_names) {
      index = nms;
    } else {
      index = PROTECT_N(Rf_allocVector(INTSXP, n_inputs), &n_prot);
      r_int_fill_seq(index, 1, n_inputs);
    }
    names_to_type = TYPEOF(index);
    names_to_col = PROTECT_N(Rf_allocVector(names_to_type, n_rows), &n_prot);

    p_index = r_vec_deref_barrier_const(index);
    p_names_to_col = r_vec_deref_barrier(names_to_col);
  }

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  const struct vec_assign_opts bind_assign_opts = {
    .assign_names = true
  };

  for (R_len_t i = 0; i < n_inputs; ++i) {
    R_len_t size = ns[i];
    if (!size) {
      continue;
    }
    SEXP x = VECTOR_ELT(xs, i);

    init_compact_seq(idx_ptr, counter, size, true);

    // Total ownership of `out` because it was freshly created with `vec_init()`
    out = df_assign(out, idx, x, VCTRS_OWNED_true, &bind_assign_opts);
    REPROTECT(out, out_pi);

    // FIXME: This work happens in parallel to the names assignment in
    // `df_assign()`. We should add a way to instruct df-assign to
    // ignore the outermost names (but still assign inner names in
    // case of data frames).
    if (has_rownames) {
      SEXP rn = df_rownames(x);

      if (has_names && nms_p[i] != strings_empty && !has_names_to) {
        if (rownames_type(rn) == ROWNAMES_IDENTIFIERS) {
          rn = apply_name_spec(name_spec, nms_p[i], rn, size);
        } else if (size > 1) {
          rn = r_seq_chr(CHAR(nms_p[i]), size);
        } else {
          rn = r_str_as_character(nms_p[i]);
        }
      }
      PROTECT(rn);

      if (rownames_type(rn) == ROWNAMES_IDENTIFIERS) {
        rownames = chr_assign(rownames, idx, rn, VCTRS_OWNED_true);
        REPROTECT(rownames, rownames_pi);
      }

      UNPROTECT(1);
    }
    PROTECT(rownames);

    // Assign current name to group vector, if supplied
    if (has_names_to) {
      r_vec_fill(names_to_type, p_names_to_col, counter, p_index, i, size);
    }

    counter += size;
    UNPROTECT(1);
  }

  if (has_rownames) {
    const struct name_repair_opts rownames_repair = {
      .type = name_repair_unique,
      .fn = R_NilValue,
      .quiet = true
    };
    rownames = vec_as_names(rownames, &rownames_repair);
    REPROTECT(rownames, rownames_pi);
    Rf_setAttrib(out, R_RowNamesSymbol, rownames);
  }

  if (has_names_to) {
    out = df_poke(out, names_to_loc, names_to_col);
    REPROTECT(out, out_pi);
  }

  // Not optimal. Happens after the fallback columns have been
  // assigned already, ideally they should be ignored. Also this is
  // currently not recursive. Should we deal with this during
  // restoration?
  for (R_len_t i = 0; i < n_cols; ++i) {
    SEXP col = r_list_get(ptype, i);

    if (vec_is_common_class_fallback(col)) {
      SEXP col_xs = PROTECT(list_pluck(xs, i));
      SEXP col_out = vec_c_fallback(col, col_xs, name_spec, name_repair);
      r_list_poke(out, i, col_out);
      UNPROTECT(1);
    }
  }

  out = vec_restore(out, ptype, PROTECT(r_int(n_rows)), VCTRS_OWNED_true);

  UNPROTECT(n_prot);
  UNPROTECT(4);
  return out;
}

static SEXP as_df_row(SEXP x, struct name_repair_opts* name_repair) {
  if (vec_is_unspecified(x) && r_names(x) == R_NilValue) {
    return x;
  } else {
    return as_df_row_impl(x, name_repair);
  }
}

static SEXP as_df_row_impl(SEXP x, struct name_repair_opts* name_repair) {
  if (x == R_NilValue) {
    return x;
  }
  if (is_data_frame(x)) {
    return df_repair_names(x, name_repair);
  }

  int nprot = 0;

  R_len_t ndim = vec_dim_n(x);
  if (ndim > 2) {
    Rf_errorcall(R_NilValue, "Can't bind arrays.");
  }
  if (ndim == 2) {
    SEXP names = PROTECT_N(vec_unique_colnames(x, name_repair->quiet), &nprot);
    SEXP out = PROTECT_N(r_as_data_frame(x), &nprot);
    r_poke_names(out, names);
    UNPROTECT(nprot);
    return out;
  }

  SEXP nms = PROTECT_N(vec_names(x), &nprot);

  // Remove names as they are promoted to data frame column names
  if (nms != R_NilValue) {
    x = PROTECT_N(r_clone_referenced(x), &nprot);
    r_poke_names(x, R_NilValue);
  }

  if (nms == R_NilValue) {
    nms = PROTECT_N(vec_unique_names(x, name_repair->quiet), &nprot);
  } else {
    nms = PROTECT_N(vec_as_names(nms, name_repair), &nprot);
  }

  x = PROTECT_N(vec_chop(x, R_NilValue), &nprot);

  r_poke_names(x, nms);

  x = new_data_frame(x, 1);

  UNPROTECT(nprot);
  return x;
}

// [[ register() ]]
SEXP vctrs_as_df_row(SEXP x, SEXP quiet) {
  struct name_repair_opts name_repair_opts = {
    .type = name_repair_unique,
    .fn = R_NilValue,
    .quiet = LOGICAL(quiet)[0]
  };
  return as_df_row(x, &name_repair_opts);
}

static SEXP cbind_names_to(bool has_names, SEXP names_to, SEXP ptype) {
  SEXP index_ptype = has_names ? vctrs_shared_empty_chr : vctrs_shared_empty_int;

  SEXP tmp = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(tmp, 0, index_ptype);
  SET_VECTOR_ELT(tmp, 1, ptype);

  SEXP tmp_nms = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(tmp_nms, 0, names_to);
  SET_STRING_ELT(tmp_nms, 1, strings_empty);

  r_poke_names(tmp, tmp_nms);

  SEXP out = vec_cbind(tmp, R_NilValue, R_NilValue, NULL);

  UNPROTECT(2);
  return out;
}


static SEXP as_df_col(SEXP x, SEXP outer, bool* allow_pack);
static SEXP cbind_container_type(SEXP x, void* data);

// [[ register(external = TRUE) ]]
SEXP vctrs_cbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP size = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  struct name_repair_opts name_repair_opts = validate_bind_name_repair(name_repair, true);
  PROTECT_NAME_REPAIR_OPTS(&name_repair_opts);

  SEXP out = vec_cbind(xs, ptype, size, &name_repair_opts);

  UNPROTECT(5);
  return out;
}

static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, struct name_repair_opts* name_repair) {
  R_len_t n = Rf_length(xs);

  // Find the common container type of inputs
  SEXP rownames = R_NilValue;
  SEXP containers = PROTECT(map_with_data(xs, &cbind_container_type, &rownames));
  ptype = PROTECT(cbind_container_type(ptype, &rownames));

  SEXP type = PROTECT(vec_ptype_common_params(containers,
                                              ptype,
                                              DF_FALLBACK_DEFAULT,
                                              S3_FALLBACK_false));
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

  if (rownames != R_NilValue && Rf_length(rownames) != nrow) {
    rownames = PROTECT(vec_recycle(rownames, nrow, args_empty));
    rownames = vec_as_unique_names(rownames, false);
    UNPROTECT(1);
  }
  PROTECT(rownames);

  // Convert inputs to data frames, validate, and collect total number of columns
  SEXP xs_names = PROTECT(r_names(xs));
  bool has_names = xs_names != R_NilValue;
  SEXP const* xs_names_p = has_names ? STRING_PTR_RO(xs_names) : NULL;

  R_len_t ncol = 0;
  for (R_len_t i = 0; i < n; ++i) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    x = PROTECT(vec_recycle(x, nrow, args_empty));

    SEXP outer_name = has_names ? xs_names_p[i] : strings_empty;
    bool allow_packing;
    x = PROTECT(as_df_col(x, outer_name, &allow_packing));

    // Remove outer name of column vectors because they shouldn't be repacked
    if (has_names && !allow_packing) {
      SET_STRING_ELT(xs_names, i, strings_empty);
    }

    SET_VECTOR_ELT(xs, i, x);
    UNPROTECT(2);

    // Named inputs are packed in a single column
    R_len_t x_ncol = outer_name == strings_empty ? Rf_length(x) : 1;
    ncol += x_ncol;
  }


  // Fill in columns
  PROTECT_INDEX out_pi;
  SEXP out = Rf_allocVector(VECSXP, ncol);
  PROTECT_WITH_INDEX(out, &out_pi);

  PROTECT_INDEX names_pi;
  SEXP names = Rf_allocVector(STRSXP, ncol);
  PROTECT_WITH_INDEX(names, &names_pi);

  SEXP idx = PROTECT(compact_seq(0, 0, true));
  int* idx_ptr = INTEGER(idx);

  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    SEXP outer_name = has_names ? xs_names_p[i] : strings_empty;
    if (outer_name != strings_empty) {
      SET_VECTOR_ELT(out, counter, x);
      SET_STRING_ELT(names, counter, outer_name);
      ++counter;
      continue;
    }

    R_len_t xn = Rf_length(x);
    init_compact_seq(idx_ptr, counter, xn, true);

    // Total ownership of `out` because it was freshly created with `Rf_allocVector()`
    out = list_assign(out, idx, x, VCTRS_OWNED_true);
    REPROTECT(out, out_pi);

    SEXP xnms = PROTECT(r_names(x));
    if (xnms != R_NilValue) {
      names = chr_assign(names, idx, xnms, VCTRS_OWNED_true);
      REPROTECT(names, names_pi);
    }
    UNPROTECT(1);

    counter += xn;
  }

  names = PROTECT(vec_as_names(names, name_repair));
  Rf_setAttrib(out, R_NamesSymbol, names);

  if (rownames != R_NilValue) {
    Rf_setAttrib(out, R_RowNamesSymbol, rownames);
  }

  out = vec_restore(out, type, R_NilValue, VCTRS_OWNED_true);

  UNPROTECT(9);
  return out;
}

SEXP syms_vec_cbind_frame_ptype = NULL;
SEXP fns_vec_cbind_frame_ptype = NULL;

SEXP vec_cbind_frame_ptype(SEXP x) {
  return vctrs_dispatch1(syms_vec_cbind_frame_ptype,
                         fns_vec_cbind_frame_ptype,
                         syms_x,
                         x);
}

static SEXP cbind_container_type(SEXP x, void* data) {
  if (is_data_frame(x)) {
    SEXP rn = df_rownames(x);

    if (rownames_type(rn) == ROWNAMES_IDENTIFIERS) {
      SEXP* learned_rn_p = (SEXP*) data;
      SEXP learned_rn = *learned_rn_p;

      if (learned_rn == R_NilValue) {
        *learned_rn_p = rn;
      }
    }

    return vec_cbind_frame_ptype(x);
  } else {
    return R_NilValue;
  }
}


static SEXP shaped_as_df_col(SEXP x, SEXP outer);
static SEXP vec_as_df_col(SEXP x, SEXP outer);

// [[ register() ]]
SEXP vctrs_as_df_col(SEXP x, SEXP outer) {
  bool allow_pack;
  return as_df_col(x, r_chr_get(outer, 0), &allow_pack);
}
static SEXP as_df_col(SEXP x, SEXP outer, bool* allow_pack) {
  if (is_data_frame(x)) {
    *allow_pack = true;
    return Rf_shallow_duplicate(x);
  }

  R_len_t ndim = vec_bare_dim_n(x);
  if (ndim > 2) {
    Rf_errorcall(R_NilValue, "Can't bind arrays.");
  }
  if (ndim > 0) {
    *allow_pack = true;
    return shaped_as_df_col(x, outer);
  }

  *allow_pack = false;
  return vec_as_df_col(x, outer);
}

static SEXP shaped_as_df_col(SEXP x, SEXP outer) {
  // If packed, store array as a column
  if (outer != strings_empty) {
    return x;
  }

  // If unpacked, transform to data frame first. We repair names
  // after unpacking and concatenation.
  SEXP out = PROTECT(r_as_data_frame(x));

  // Remove names if they were repaired by `as.data.frame()`
  if (colnames(x) == R_NilValue) {
    r_poke_names(out, R_NilValue);
  }

  UNPROTECT(1);
  return out;
}

static SEXP vec_as_df_col(SEXP x, SEXP outer) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(out, 0, x);

  if (outer != strings_empty) {
    SEXP names = PROTECT(r_str_as_character(outer));
    Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(1);
  }

  init_data_frame(out, Rf_length(x));

  UNPROTECT(1);
  return out;
}

struct name_repair_opts validate_bind_name_repair(SEXP name_repair, bool allow_minimal) {
  struct name_repair_opts opts = new_name_repair_opts(name_repair, args_empty, false);

  switch (opts.type) {
  case name_repair_custom:
  case name_repair_unique:
  case name_repair_universal:
  case name_repair_check_unique:
    break;
  case name_repair_minimal:
    if (allow_minimal) break; // else fallthrough
  default:
    if (allow_minimal) {
      Rf_errorcall(R_NilValue,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, `\"check_unique\"`, or `\"minimal\"`.",
                   name_repair_arg_as_c_string(opts.type));
    } else {
      Rf_errorcall(R_NilValue,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, or `\"check_unique\"`.",
                   name_repair_arg_as_c_string(opts.type));
    }
  }

  return opts;
}

void vctrs_init_bind(SEXP ns) {
  syms_vec_cbind_frame_ptype = Rf_install("vec_cbind_frame_ptype");
  fns_vec_cbind_frame_ptype = r_env_get(ns, syms_vec_cbind_frame_ptype);
}
