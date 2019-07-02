#include "vctrs.h"
#include "utils.h"

// Initialised at load time
static SEXP syms_vec_cast_dispatch = NULL;
static SEXP syms_vec_restore_dispatch = NULL;
static SEXP syms_df_lossy_cast = NULL;
static SEXP fns_vec_cast_dispatch = NULL;
static SEXP fns_vec_restore_dispatch = NULL;
static SEXP fns_df_lossy_cast = NULL;


static SEXP int_as_logical(SEXP x, bool* lossy) {
  int* data = INTEGER(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* out_data = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;

    if (elt == NA_INTEGER) {
      *out_data = NA_LOGICAL;
      continue;
    }

    if (elt != 0 && elt != 1) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    *out_data = elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP dbl_as_logical(SEXP x, bool* lossy) {
  double* data = REAL(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* out_data = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    double elt = *data;

    if (isnan(elt)) {
      *out_data = NA_LOGICAL;
      continue;
    }

    if (elt != 0 && elt != 1) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    *out_data = (int) elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP chr_as_logical(SEXP x, bool* lossy) {
  SEXP* data = STRING_PTR(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* out_data = LOGICAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    SEXP str = *data;
    if (str == NA_STRING) {
      *out_data = NA_LOGICAL;
      continue;
    }

    const char* elt = CHAR(str);
    switch (elt[0]) {
    case 'T':
      if (elt[1] == '\0' || strcmp(elt, "TRUE") == 0) {
        *out_data = 1;
        continue;
      }
      break;
    case 'F':
      if (elt[1] == '\0' || strcmp(elt, "FALSE") == 0) {
        *out_data = 0;
        continue;
      }
      break;
    case 't':
      if (strcmp(elt, "true") == 0) {
        *out_data = 1;
        continue;
      }
      break;
    case 'f':
      if (strcmp(elt, "false") == 0) {
        *out_data = 0;
        continue;
      }
      break;
    default:
      break;
    }

    *lossy = true;
    UNPROTECT(1);
    return R_NilValue;
  }

  UNPROTECT(1);
  return out;
}

static SEXP lgl_as_integer(SEXP x, bool* lossy) {
  return Rf_coerceVector(x, INTSXP);
}

static SEXP dbl_as_integer(SEXP x, bool* lossy) {
  double* data = REAL(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int* out_data = INTEGER(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    double elt = *data;

    if (elt <= INT_MIN || elt >= INT_MAX + 1.0) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    if (isnan(elt)) {
      *out_data = NA_INTEGER;
      continue;
    }

    int value = (int) elt;

    if (value != elt) {
      *lossy = true;
      UNPROTECT(1);
      return R_NilValue;
    }

    *out_data = value;
  }

  UNPROTECT(1);
  return out;
}

static SEXP lgl_as_double(SEXP x, bool* lossy) {
  int* data = LOGICAL(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double* out_data = REAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;
    *out_data = (elt == NA_LOGICAL) ? NA_REAL : elt;
  }

  UNPROTECT(1);
  return out;
}

static SEXP int_as_double(SEXP x, bool* lossy) {
  int* data = INTEGER(x);
  R_len_t n = Rf_length(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double* out_data = REAL(out);

  for (R_len_t i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;
    *out_data = (elt == NA_INTEGER) ? NA_REAL : elt;
  }

  UNPROTECT(1);
  return out;
}


// From dictionary.c
SEXP vctrs_match(SEXP needles, SEXP haystack);

// Defined below
static SEXP df_as_dataframe(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

// [[ register() ]]
SEXP vctrs_df_as_dataframe(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  if (!r_is_string(x_arg_)) {
    Rf_errorcall(R_NilValue, "`x_arg` must be a string");
  }
  if (!r_is_string(to_arg_)) {
    Rf_errorcall(R_NilValue, "`to_arg` must be a string");
  }

  struct vctrs_arg x_arg = new_wrapper_arg(NULL, r_chr_get_c_string(x_arg_, 0));
  struct vctrs_arg to_arg = new_wrapper_arg(NULL, r_chr_get_c_string(to_arg_, 0));

  return df_as_dataframe(x, to, &x_arg, &to_arg);
}

// Take all columns of `to` and preserve the order. Common columns are
// cast to their types in `to`. Extra `x` columns are dropped and
// cause a lossy cast. Extra `to` columns are filled with missing
// values.
static SEXP df_as_dataframe(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  SEXP x_names = PROTECT(r_names(x));
  SEXP to_names = PROTECT(r_names(to));

  if (x_names == R_NilValue || to_names == R_NilValue) {
    Rf_error("Internal error in `df_as_dataframe()`: Data frame must have names.");
  }

  SEXP to_dups_pos = PROTECT(vctrs_match(to_names, x_names));
  int* to_dups_pos_data = INTEGER(to_dups_pos);

  R_len_t to_len = Rf_length(to_dups_pos);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, to_len));
  Rf_setAttrib(out, R_NamesSymbol, to_names);

  R_len_t size = df_size(x);
  R_len_t common_len = 0;

  for (R_len_t i = 0; i < to_len; ++i) {
    R_len_t pos = to_dups_pos_data[i];

    SEXP col;
    if (pos == NA_INTEGER) {
      col = vec_init(VECTOR_ELT(to, i), size);
    } else {
      --pos; // 1-based index
      struct arg_data_index x_arg_data = new_index_arg_data(r_chr_get_c_string(x_names, pos), x_arg);
      struct arg_data_index to_arg_data = new_index_arg_data(r_chr_get_c_string(to_names, i), to_arg);
      struct vctrs_arg named_x_arg = new_index_arg(x_arg, &x_arg_data);
      struct vctrs_arg named_to_arg = new_index_arg(to_arg, &to_arg_data);
      ++common_len;
      col = vec_cast(VECTOR_ELT(x, pos), VECTOR_ELT(to, i), &named_x_arg, &named_to_arg);
    }

    SET_VECTOR_ELT(out, i, col);
  }

  // Restore data frame size before calling `vec_restore()`. `x` and
  // `to` might not have any columns to compute the original size.
  init_data_frame(out, size);

  out = PROTECT(vec_restore(out, to, R_NilValue));

  R_len_t extra_len = Rf_length(x) - common_len;
  if (extra_len) {
    out = vctrs_dispatch3(syms_df_lossy_cast, fns_df_lossy_cast,
                          syms_out, out,
                          syms_x, x,
                          syms_to, to);
  }

  UNPROTECT(5);
  return out;
}

static SEXP vec_cast_switch(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  switch (vec_typeof(to)) {
  case vctrs_type_logical:
    switch (vec_typeof(x)) {
    case vctrs_type_logical:
      return x;
    case vctrs_type_integer:
      return int_as_logical(x, lossy);
    case vctrs_type_double:
      return dbl_as_logical(x, lossy);
    case vctrs_type_character:
      return chr_as_logical(x, lossy);
    default:
      break;
    }
    break;

  case vctrs_type_integer:
    switch (vec_typeof(x)) {
    case vctrs_type_logical:
      return lgl_as_integer(x, lossy);
    case vctrs_type_integer:
      return x;
    case vctrs_type_double:
      return dbl_as_integer(x, lossy);
    case vctrs_type_character:
      // TODO: Implement with `R_strtod()` from R_ext/utils.h
      break;
    default:
      break;
    }
    break;

  case vctrs_type_double:
    switch (vec_typeof(x)) {
    case vctrs_type_logical:
      return lgl_as_double(x, lossy);
    case vctrs_type_integer:
      return int_as_double(x, lossy);
    case vctrs_type_double:
      return x;
    case vctrs_type_character:
      // TODO: Implement with `R_strtod()` from R_ext/utils.h
      break;
    default:
      break;
    }
    break;

  case vctrs_type_character:
    switch (vec_typeof(x)) {
    case vctrs_type_logical:
    case vctrs_type_integer:
    case vctrs_type_double:
      return Rf_coerceVector(x, STRSXP);
    case vctrs_type_character:
      return x;
    default:
      break;
    }
    break;

  case vctrs_type_dataframe:
    switch (vec_typeof(x)) {
    case vctrs_type_dataframe:
      return df_as_dataframe(x, to, x_arg, to_arg);
    default:
      break;
    }

  default:
    break;
  }

  return R_NilValue;
}

// [[ register() ]]
SEXP vctrs_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  if (!r_is_string(x_arg_)) {
    Rf_errorcall(R_NilValue, "`x_arg` must be a string");
  }
  if (!r_is_string(to_arg_)) {
    Rf_errorcall(R_NilValue, "`to_arg` must be a string");
  }

  struct vctrs_arg x_arg = new_wrapper_arg(NULL, r_chr_get_c_string(x_arg_, 0));
  struct vctrs_arg to_arg = new_wrapper_arg(NULL, r_chr_get_c_string(to_arg_, 0));

  return vec_cast(x, to, &x_arg, &to_arg);
}

// [[ include("vctrs.h") ]]
SEXP vec_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  if (x == R_NilValue || to == R_NilValue) {
    return x;
  }

  bool lossy = false;
  SEXP out = R_NilValue;

  if (!has_dim(x) && !has_dim(to)) {
    out = vec_cast_switch(x, to, &lossy, x_arg, to_arg);
  }

  if (!lossy && out != R_NilValue) {
    return out;
  }

  out = vctrs_dispatch4(syms_vec_cast_dispatch, fns_vec_cast_dispatch,
                        syms_x, x,
                        syms_to, to,
                        syms_x_arg, PROTECT(vctrs_arg(x_arg)),
                        syms_to_arg, PROTECT(vctrs_arg(to_arg)));
  UNPROTECT(2);
  return out;
}

// Copy attributes except names and dim. This duplicates `x` if needed.
SEXP vec_restore_default(SEXP x, SEXP to) {
  int n_protect = 0;

  SEXP attrib = PROTECT(Rf_shallow_duplicate(ATTRIB(to)));
  ++n_protect;

  if (attrib == R_NilValue) {
    UNPROTECT(n_protect);
    return x;
  }

  if (MAYBE_REFERENCED(x)) {
    x = PROTECT(Rf_shallow_duplicate(x));
    ++n_protect;
  }

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

      if (tag == R_NamesSymbol || tag == R_DimSymbol ||
          tag == R_DimNamesSymbol || tag == R_ClassSymbol ||
          tag == R_RowNamesSymbol) {
        if (tag == R_ClassSymbol) {
          class = CAR(node);
        }
        if (prev == R_NilValue) {
          attrib = CDR(attrib);
          node = CDR(node);
          continue;
        }

        SETCDR(prev, CDR(node));
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
    SEXP rownms = PROTECT(Rf_getAttrib(x, R_RowNamesSymbol));

    SET_ATTRIB(x, attrib);

    Rf_setAttrib(x, R_NamesSymbol, nms);
    Rf_setAttrib(x, R_RowNamesSymbol, rownms);
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

  UNPROTECT(n_protect);
  return x;
}

SEXP vctrs_df_restore(SEXP x, SEXP to, SEXP n) {
  if (TYPEOF(x) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: Attempt to restore data frame from a %s.",
                 Rf_type2char(TYPEOF(x)));
  }

  R_len_t size = (n == R_NilValue) ? df_raw_size(x) : r_int_get(n, 0);
  return df_restore_impl(x, to, size);
}

SEXP df_restore_impl(SEXP x, SEXP to, R_len_t size) {
  x = PROTECT(r_maybe_duplicate(x));
  x = PROTECT(vec_restore_default(x, to));

  if (Rf_getAttrib(x, R_NamesSymbol) == R_NilValue) {
    Rf_setAttrib(x, R_NamesSymbol, vctrs_shared_empty_chr);
  }

  SEXP rownames = PROTECT(Rf_getAttrib(x, R_RowNamesSymbol));
  if (rownames == R_NilValue) {
    rownames = PROTECT(Rf_allocVector(INTSXP, 2));

    INTEGER(rownames)[0] = NA_INTEGER;
    INTEGER(rownames)[1] = -size;

    Rf_setAttrib(x, R_RowNamesSymbol, rownames);
    UNPROTECT(1);
  }

  UNPROTECT(3);
  return x;
}


static SEXP vec_restore_dispatch(SEXP x, SEXP to, SEXP n);

SEXP vec_restore(SEXP x, SEXP to, SEXP n) {
  switch (class_type(to)) {
  default: return vec_restore_dispatch(x, to, n);
  case vctrs_class_none: return vec_restore_default(x, to);
  case vctrs_class_bare_data_frame:
  case vctrs_class_bare_tibble: return vctrs_df_restore(x, to, n);
  case vctrs_class_data_frame: {
    // Restore methods are passed the original atomic type back, so we
    // first restore data frames as such before calling the restore
    // method, if any
    SEXP out = PROTECT(vctrs_df_restore(x, to, n));
    out = vec_restore_dispatch(x, to, n);
    UNPROTECT(1);
    return out;
  }}
}

static SEXP vec_restore_dispatch(SEXP x, SEXP to, SEXP n) {
  return vctrs_dispatch3(syms_vec_restore_dispatch, fns_vec_restore_dispatch,
                         syms_x, x,
                         syms_to, to,
                         syms_n, n);
}


// [[ include("vctrs.h") ]]
SEXP vec_coercible_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  // Called for the side effect of generating an error if there is no
  // common type
  int _left;
  vec_type2(x, to, x_arg, to_arg, &_left);

  return vec_cast(x, to, x_arg, to_arg);
}

// [[ register() ]]
SEXP vctrs_coercible_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  if (!r_is_string(x_arg_)) {
    Rf_errorcall(R_NilValue, "`x_arg` must be a string");
  }
  if (!r_is_string(to_arg_)) {
    Rf_errorcall(R_NilValue, "`to_arg` must be a string");
  }

  struct vctrs_arg x_arg = new_wrapper_arg(NULL, r_chr_get_c_string(x_arg_, 0));
  struct vctrs_arg to_arg = new_wrapper_arg(NULL, r_chr_get_c_string(to_arg_, 0));

  return vec_coercible_cast(x, to, &x_arg, &to_arg);
}


SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);

// [[ include("vctrs.h") ]]
SEXP vec_cast_common(SEXP xs, SEXP to) {
  SEXP type = PROTECT(vctrs_type_common_impl(xs, to));

  R_len_t n = Rf_length(xs);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    // TODO
    SET_VECTOR_ELT(out, i, vec_cast(elt, type, args_empty, args_empty));
  }

  SEXP names = PROTECT(Rf_getAttrib(xs, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(3);
  return out;
}

// [[ register(external = TRUE) ]]
SEXP vctrs_cast_common(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP dots = PROTECT(rlang_env_dots_list(env));
  SEXP to = PROTECT(Rf_eval(CAR(args), env));

  SEXP out = vec_cast_common(dots, to);

  UNPROTECT(2);
  return out;
}


void vctrs_init_cast(SEXP ns) {
  syms_vec_cast_dispatch = Rf_install("vec_cast_dispatch");
  syms_vec_restore_dispatch = Rf_install("vec_restore_dispatch");
  syms_df_lossy_cast = Rf_install("df_lossy_cast");

  fns_vec_cast_dispatch = Rf_findVar(syms_vec_cast_dispatch, ns);
  fns_vec_restore_dispatch = Rf_findVar(syms_vec_restore_dispatch, ns);
  fns_df_lossy_cast = Rf_findVar(syms_df_lossy_cast, ns);
}
