#include "vctrs.h"
#include "type-data-frame.h"

static SEXP syms_df_lossy_cast = NULL;
static SEXP fns_df_lossy_cast = NULL;

static SEXP new_compact_rownames(R_len_t n);


// [[ include("type-data-frame.h") ]]
bool is_data_frame(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    return false;
  }

  enum vctrs_class_type type = class_type(x);
  return
    type == VCTRS_CLASS_bare_data_frame ||
    type == VCTRS_CLASS_bare_tibble ||
    type == VCTRS_CLASS_data_frame;
}

// [[ include("type-data-frame.h") ]]
bool is_native_df(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == VCTRS_CLASS_bare_data_frame ||
    type == VCTRS_CLASS_bare_tibble;
}

// [[ include("type-data-frame.h") ]]
bool is_bare_data_frame(SEXP x) {
  return class_type(x) == VCTRS_CLASS_bare_data_frame;
}

// [[ include("type-data-frame.h") ]]
bool is_bare_tibble(SEXP x) {
  return class_type(x) == VCTRS_CLASS_bare_tibble;
}

// [[ include("type-data-frame.h") ]]
SEXP new_data_frame(SEXP x, R_len_t n) {
  x = PROTECT(r_clone_referenced(x));
  init_data_frame(x, n);

  UNPROTECT(1);
  return x;
}

static R_len_t df_size_from_list(SEXP x, SEXP n);
static R_len_t df_size_from_n(SEXP n);
static SEXP c_data_frame_class(SEXP cls);

// [[ register() ]]
SEXP vctrs_new_data_frame(SEXP args) {
  args = CDR(args);

  SEXP x = CAR(args); args = CDR(args);
  SEXP n = CAR(args); args = CDR(args);
  SEXP cls = CAR(args); args = CDR(args);
  SEXP attrib = args;

  PROTECT_INDEX pi;
  PROTECT_WITH_INDEX(attrib, &pi);

  if (TYPEOF(x) != VECSXP) {
    Rf_errorcall(R_NilValue, "`x` must be a list");
  }

  bool has_names = false;
  bool has_rownames = false;
  R_len_t size = df_size_from_list(x, n);

  SEXP out = PROTECT(r_clone_referenced(x));

  for (SEXP node = attrib; node != R_NilValue; node = CDR(node)) {
    SEXP tag = TAG(node);

    // We might add dynamic dots later on
    if (tag == R_ClassSymbol) {
      r_stop_internal("Can't supply `class` in `...`.");
    }

    if (tag == R_NamesSymbol) {
      has_names = true;
      continue;
    }

    if (tag == R_RowNamesSymbol) {
      // "row.names" is checked for consistency with n (if provided)
      if (size != rownames_size(CAR(node)) && n != R_NilValue) {
        Rf_errorcall(R_NilValue, "`n` and `row.names` must be consistent.");
      }

      has_rownames = true;
      continue;
    }
  }

  // Take names from `x` if `attrib` doesn't have any
  if (!has_names) {
    SEXP nms = vctrs_shared_empty_chr;
    if (Rf_length(out)) {
      nms = r_names(out);
    }
    PROTECT(nms);

    if (nms != R_NilValue) {
      attrib = Rf_cons(nms, attrib);
      SET_TAG(attrib, R_NamesSymbol);
      REPROTECT(attrib, pi);
    }

    UNPROTECT(1);
  }

  if (!has_rownames) {
    SEXP rn = PROTECT(new_compact_rownames(size));
    attrib = Rf_cons(rn, attrib);
    SET_TAG(attrib, R_RowNamesSymbol);

    UNPROTECT(1);
    REPROTECT(attrib, pi);
  }

  if (cls == R_NilValue) {
    cls = classes_data_frame;
  } else {
    cls = c_data_frame_class(cls);
  }
  PROTECT(cls);

  attrib = Rf_cons(cls, attrib);
  SET_TAG(attrib, R_ClassSymbol);

  UNPROTECT(1);
  REPROTECT(attrib, pi);


  SET_ATTRIB(out, attrib);
  SET_OBJECT(out, 1);

  UNPROTECT(2);
  return out;
}

static R_len_t df_size_from_list(SEXP x, SEXP n) {
  if (n == R_NilValue) {
    if (is_data_frame(x)) {
      return df_size(x);
    } else {
      return df_raw_size_from_list(x);
    }
  } else {
    return df_size_from_n(n);
  }
}

static R_len_t df_size_from_n(SEXP n) {
  if (TYPEOF(n) != INTSXP || Rf_length(n) != 1) {
    r_abort("`n` must be an integer of size 1.");
  }

  R_len_t out = r_int_get(n, 0);

  if (out == r_globals.na_int) {
    r_abort("`n` can't be missing.");
  }
  if (out < 0) {
    r_abort("`n` can't be negative.");
  }

  return out;
}

static SEXP c_data_frame_class(SEXP cls) {
  if (TYPEOF(cls) != STRSXP) {
    Rf_errorcall(R_NilValue, "`class` must be NULL or a character vector");
  }
  return chr_c(cls, classes_data_frame);
}


SEXP data_frame(SEXP x, r_ssize size, const struct name_repair_opts* p_name_repair_opts);

// [[ register() ]]
SEXP vctrs_data_frame(SEXP x, SEXP size, SEXP name_repair) {
  // TODO! call
  struct r_lazy call = r_lazy_null;

  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair,
                                                                  args_empty,
                                                                  false,
                                                                  call);
  KEEP(name_repair_opts.shelter);

  r_ssize c_size = 0;
  if (size == R_NilValue) {
    struct size_common_opts opts = { .call = call };
    c_size = vec_size_common_opts(x, 0, &opts);
  } else {
    c_size = vec_as_short_length(size, vec_args.dot_size, call);
  }

  SEXP out = data_frame(x, c_size, &name_repair_opts);

  FREE(1);
  return out;
}

SEXP df_list(SEXP x, r_ssize size, const struct name_repair_opts* p_name_repair_opts);

SEXP data_frame(SEXP x, r_ssize size, const struct name_repair_opts* p_name_repair_opts) {
  SEXP out = PROTECT(df_list(x, size, p_name_repair_opts));
  out = new_data_frame(out, size);
  UNPROTECT(1);
  return out;
}


// [[ register() ]]
SEXP vctrs_df_list(SEXP x, SEXP size, SEXP name_repair) {
  // TODO! call
  struct r_lazy call = r_lazy_null;

  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair,
                                                                  args_empty,
                                                                  false,
                                                                  call);
  KEEP(name_repair_opts.shelter);

  r_ssize c_size = 0;
  if (size == R_NilValue) {
    struct size_common_opts opts = { .call = call };
    c_size = vec_size_common_opts(x, 0, &opts);
  } else {
    c_size = vec_as_short_length(size, vec_args.dot_size, call);
  }

  SEXP out = df_list(x, c_size, &name_repair_opts);

  FREE(1);
  return out;
}

static SEXP df_list_drop_null(SEXP x);
static SEXP df_list_splice(SEXP x);

SEXP df_list(SEXP x, r_ssize size, const struct name_repair_opts* p_name_repair_opts) {
  // TODO! call
  struct r_lazy call = r_lazy_null;

  if (TYPEOF(x) != VECSXP) {
    r_stop_internal("`x` must be a list.");
  }

  struct size_common_opts size_opts = { .call = call };
  x = PROTECT(vec_recycle_common_opts(x, size, &size_opts));

  r_ssize n_cols = r_length(x);

  // Unnamed columns are auto-named with `""`
  if (r_names(x) == R_NilValue) {
    SEXP names = PROTECT(r_new_character(n_cols));
    r_attrib_poke_names(x, names);
    UNPROTECT(1);
  }

  x = PROTECT(df_list_drop_null(x));
  x = PROTECT(df_list_splice(x));

  SEXP names = PROTECT(r_names(x));
  names = PROTECT(vec_as_names(names, p_name_repair_opts));
  r_attrib_poke_names(x, names);

  UNPROTECT(5);
  return x;
}

static SEXP df_list_drop_null(SEXP x) {
  r_ssize n_cols = r_length(x);
  r_ssize count = 0;

  for (r_ssize i = 0; i < n_cols; ++i) {
    count += VECTOR_ELT(x, i) == R_NilValue;
  }

  if (count == 0) {
    return x;
  }

  SEXP names = PROTECT(r_names(x));
  const SEXP* p_names = STRING_PTR_RO(names);

  r_ssize n_out = n_cols - count;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_out));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, n_out));
  r_ssize out_i = 0;

  for (r_ssize i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);

    if (col != R_NilValue) {
      SET_VECTOR_ELT(out, out_i, col);
      SET_STRING_ELT(out_names, out_i, p_names[i]);
      ++out_i;
    }
  }

  r_attrib_poke_names(out, out_names);

  UNPROTECT(3);
  return out;
}

static SEXP df_list_splice(SEXP x) {
  SEXP names = PROTECT(r_names(x));
  const SEXP* p_names = STRING_PTR_RO(names);

  bool any_needs_splice = false;
  r_ssize n_cols = r_length(x);
  r_ssize i = 0;

  for (; i < n_cols; ++i) {
    // Only splice unnamed data frames
    if (p_names[i] != strings_empty) {
      continue;
    }

    SEXP col = VECTOR_ELT(x, i);

    if (is_data_frame(col)) {
      any_needs_splice = true;
      break;
    }
  }

  if (!any_needs_splice) {
    UNPROTECT(1);
    return x;
  }

  SEXP splice = PROTECT(r_new_logical(n_cols));
  int* p_splice = LOGICAL(splice);

  for (r_ssize j = 0; j < n_cols; ++j) {
    p_splice[j] = 0;
  }

  r_ssize width = i;

  for (; i < n_cols; ++i) {
    // Only splice unnamed data frames
    if (p_names[i] != strings_empty) {
      ++width;
      continue;
    }

    SEXP col = VECTOR_ELT(x, i);

    if (is_data_frame(col)) {
      width += r_length(col);
      p_splice[i] = 1;
    } else {
      ++width;
    }
  }

  SEXP out = PROTECT(r_new_list(width));
  SEXP out_names = PROTECT(r_new_character(width));

  r_ssize loc = 0;

  // Splice loop
  for (r_ssize i = 0; i < n_cols; ++i) {
    if (!p_splice[i]) {
      SET_VECTOR_ELT(out, loc, VECTOR_ELT(x, i));
      SET_STRING_ELT(out_names, loc, p_names[i]);
      ++loc;
      continue;
    }

    SEXP col = VECTOR_ELT(x, i);
    SEXP col_names = PROTECT(r_names(col));

    if (TYPEOF(col_names) != STRSXP) {
      r_stop_internal(
        "Encountered corrupt data frame. "
        "Data frames must have character column names."
      );
    }

    const SEXP* p_col_names = STRING_PTR_RO(col_names);
    r_ssize col_i = 0;

    r_ssize stop = loc + r_length(col);

    for (; loc < stop; ++loc, ++col_i) {
      SET_VECTOR_ELT(out, loc, VECTOR_ELT(col, col_i));
      SET_STRING_ELT(out_names, loc, p_col_names[col_i]);
    }

    loc = stop;
    UNPROTECT(1);
  }

  r_attrib_poke_names(out, out_names);

  UNPROTECT(4);
  return out;
}


// [[ include("type-data-frame.h") ]]
enum rownames_type rownames_type(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP:
    return ROWNAMES_IDENTIFIERS;
  case INTSXP:
    if (Rf_length(x) == 2 && INTEGER(x)[0] == NA_INTEGER) {
      return ROWNAMES_AUTOMATIC_COMPACT;
    } else {
      return ROWNAMES_AUTOMATIC;
    }
  default:
    Rf_error("Corrupt data in `rownames_type()`: Unexpected type `%s`.",
             Rf_type2char(TYPEOF(x)));
  }
}

static R_len_t compact_rownames_length(SEXP x) {
  return abs(INTEGER(x)[1]);
}

// [[ include("type-data-frame.h") ]]
R_len_t rownames_size(SEXP rn) {
  switch (rownames_type(rn)) {
  case ROWNAMES_IDENTIFIERS:
  case ROWNAMES_AUTOMATIC:
    return Rf_length(rn);
  case ROWNAMES_AUTOMATIC_COMPACT:
    return compact_rownames_length(rn);
  }

  never_reached("rownames_size");
}

static void init_bare_data_frame(SEXP x, R_len_t n);

// [[ include("type-data-frame.h") ]]
void init_data_frame(SEXP x, R_len_t n) {
  Rf_setAttrib(x, R_ClassSymbol, classes_data_frame);
  init_bare_data_frame(x, n);
}
// [[ include("type-data-frame.h") ]]
void init_tibble(SEXP x, R_len_t n) {
  Rf_setAttrib(x, R_ClassSymbol, classes_tibble);
  init_bare_data_frame(x, n);
}

static void init_bare_data_frame(SEXP x, R_len_t n) {
  if (Rf_length(x) == 0) {
    Rf_setAttrib(x, R_NamesSymbol, vctrs_shared_empty_chr);
  }

  init_compact_rownames(x, n);
}

// [[ include("type-data-frame.h") ]]
void init_compact_rownames(SEXP x, R_len_t n) {
  SEXP rn = PROTECT(new_compact_rownames(n));
  Rf_setAttrib(x, R_RowNamesSymbol, rn);
  UNPROTECT(1);
}

static SEXP new_compact_rownames(R_len_t n) {
  if (n <= 0) {
    return vctrs_shared_empty_int;
  }

  SEXP out = Rf_allocVector(INTSXP, 2);
  int* out_data = INTEGER(out);
  out_data[0] = NA_INTEGER;
  out_data[1] = -n;
  return out;
}


// vctrs type methods ------------------------------------------------

// [[ register() ]]
SEXP vctrs_df_ptype2_opts(SEXP x, SEXP y, SEXP opts, SEXP frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  const struct ptype2_opts c_opts = new_ptype2_opts(x, y, &x_arg, &y_arg, opts);

  return df_ptype2(&c_opts);
}

static
SEXP df_ptype2_match(const struct ptype2_opts* opts,
                     SEXP x_names,
                     SEXP y_names);

static
SEXP df_ptype2_loop(const struct ptype2_opts* opts,
                    SEXP y_names);

// [[ include("type-data-frame.h") ]]
SEXP df_ptype2(const struct ptype2_opts* opts) {
  SEXP x_names = PROTECT(r_names(opts->x));
  SEXP y_names = PROTECT(r_names(opts->y));

  SEXP out = R_NilValue;

  if (equal_object(x_names, y_names)) {
    out = df_ptype2_loop(opts, x_names);
  } else {
    out = df_ptype2_match(opts, x_names, y_names);
  }

  UNPROTECT(2);
  return out;
}

SEXP df_ptype2_match(const struct ptype2_opts* opts,
                     SEXP x_names,
                     SEXP y_names) {
  SEXP x = opts->x;
  SEXP y = opts->y;

  SEXP x_dups_pos = PROTECT(vec_match(x_names, y_names));
  SEXP y_dups_pos = PROTECT(vec_match(y_names, x_names));

  int* x_dups_pos_data = INTEGER(x_dups_pos);
  int* y_dups_pos_data = INTEGER(y_dups_pos);

  R_len_t x_len = Rf_length(x_names);
  R_len_t y_len = Rf_length(y_names);

  // Count columns that are only in `y`
  R_len_t rest_len = 0;
  for (R_len_t i = 0; i < y_len; ++i) {
    if (y_dups_pos_data[i] == NA_INTEGER) {
      ++rest_len;
    }
  }

  R_len_t out_len = x_len + rest_len;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, out_len));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, out_len));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  r_ssize i = 0;
  r_ssize y_arg_loc = 0;
  struct vctrs_arg* named_x_arg = new_subscript_arg(opts->p_x_arg, x_names, x_len, &i);
  KEEP(named_x_arg->shelter);

  struct vctrs_arg* named_y_arg = new_subscript_arg(opts->p_y_arg, y_names, y_len, &y_arg_loc);
  KEEP(named_y_arg->shelter);

  // Fill in prototypes of all the columns that are in `x`, in order
  for (; i < x_len; ++i) {
    R_len_t dup = x_dups_pos_data[i];

    SEXP col = VECTOR_ELT(x, i);
    struct ptype2_opts col_opts = *opts;
    col_opts.x = col;
    col_opts.p_x_arg = named_x_arg;

    SEXP type;
    if (dup == NA_INTEGER) {
      col_opts.y = vctrs_shared_empty_uns;
      col_opts.p_y_arg = NULL;
      type = vec_ptype2_from_unspecified(&col_opts,
                                         vec_typeof(col),
                                         col,
                                         named_x_arg);
    } else {
      // 1-based index
      --dup;
      y_arg_loc = dup;

      col_opts.y = VECTOR_ELT(y, dup);
      col_opts.p_y_arg = named_y_arg;

      int _left;
      type = vec_ptype2_opts(&col_opts, &_left);
    }

    SET_VECTOR_ELT(out, i, type);
    SET_STRING_ELT(nms, i, STRING_ELT(x_names, i));
  }

  // Fill in prototypes of the columns that are only in `y`
  for (R_len_t j = 0; i < out_len; ++j) {
    R_len_t dup = y_dups_pos_data[j];

    if (dup == NA_INTEGER) {
      SEXP col = VECTOR_ELT(y, j);
      y_arg_loc = j;

      struct ptype2_opts col_opts = *opts;
      col_opts.y = col;
      col_opts.p_y_arg = named_y_arg;
      col_opts.x = vctrs_shared_empty_uns;
      col_opts.p_x_arg = NULL;
      SEXP type = vec_ptype2_from_unspecified(&col_opts,
                                              vec_typeof(col),
                                              col,
                                              named_y_arg);

      SET_VECTOR_ELT(out, i, type);
      SET_STRING_ELT(nms, i, STRING_ELT(y_names, j));
      ++i;
    }
  }

  init_data_frame(out, 0);

  UNPROTECT(6);
  return out;
}

static
SEXP df_ptype2_loop(const struct ptype2_opts* opts,
                    SEXP names) {
  SEXP x = opts->x;
  SEXP y = opts->y;

  R_len_t len = Rf_length(names);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, len));
  Rf_setAttrib(out, R_NamesSymbol, names);

  r_ssize i = 0;
  struct vctrs_arg* named_x_arg = new_subscript_arg_vec(opts->p_x_arg, out, &i);
  struct vctrs_arg* named_y_arg = new_subscript_arg_vec(opts->p_y_arg, out, &i);

  for (; i < len; ++i) {
    struct ptype2_opts col_opts = *opts;
    col_opts.x = VECTOR_ELT(x, i);
    col_opts.y = VECTOR_ELT(y, i);
    col_opts.p_x_arg = named_x_arg;
    col_opts.p_y_arg = named_y_arg;
    int _left;

    SEXP type = vec_ptype2_opts(&col_opts, &_left);

    SET_VECTOR_ELT(out, i, type);
  }

  init_data_frame(out, 0);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_df_cast_opts(SEXP x, SEXP to, SEXP opts, SEXP frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy to_arg_ = { .x = syms.to_arg, .env = frame };
  struct vctrs_arg to_arg = new_lazy_arg(&to_arg_);

  // FIXME! Error call
  struct cast_opts c_opts = new_cast_opts(x,
                                          to,
                                          &x_arg,
                                          &to_arg,
                                          r_lazy_null,
                                          opts);

  return df_cast_opts(&c_opts);
}

static SEXP df_cast_match(const struct cast_opts* opts,
                          SEXP x_names,
                          SEXP to_names);

static SEXP df_cast_loop(const struct cast_opts* opts, SEXP names);

// Take all columns of `to` and preserve the order. Common columns are
// cast to their types in `to`. Extra `x` columns are dropped and
// cause a lossy cast. Extra `to` columns are filled with missing
// values.
// [[ include("cast.h") ]]
SEXP df_cast_opts(const struct cast_opts* opts) {
  SEXP x_names = PROTECT(r_names(opts->x));
  SEXP to_names = PROTECT(r_names(opts->to));

  if (x_names == R_NilValue || to_names == R_NilValue) {
    r_stop_internal("Data frame must have names.");
  }

  SEXP out = R_NilValue;

  if (equal_object(x_names, to_names)) {
    out = df_cast_loop(opts, x_names);
  } else {
    out = df_cast_match(opts, x_names, to_names);
  }

  UNPROTECT(2);
  return out;
}

static SEXP df_cast_match(const struct cast_opts* opts,
                          SEXP x_names,
                          SEXP to_names) {
  SEXP x = opts->x;
  SEXP to = opts->to;

  SEXP to_dups_pos = PROTECT(vec_match(to_names, x_names));
  int* to_dups_pos_data = INTEGER(to_dups_pos);

  R_len_t to_len = Rf_length(to_dups_pos);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, to_len));
  Rf_setAttrib(out, R_NamesSymbol, to_names);

  R_len_t size = df_size(x);
  R_len_t common_len = 0;

  r_ssize i = 0;
  r_ssize x_arg_loc = 0;

  struct vctrs_arg* named_x_arg = new_subscript_arg(opts->p_x_arg, x_names, r_length(x_names), &x_arg_loc);
  KEEP(named_x_arg->shelter);

  struct vctrs_arg* named_to_arg = new_subscript_arg(opts->p_to_arg, to_names, to_len, &i);
  KEEP(named_to_arg->shelter);

  for (; i < to_len; ++i) {
    R_len_t pos = to_dups_pos_data[i];

    SEXP col;
    if (pos == NA_INTEGER) {
      SEXP to_col = VECTOR_ELT(to, i);
      col = vec_init(to_col, size);

      // FIXME: Need to initialise the vector because we currently use
      // `vec_assign()` in `vec_rbind()` before falling back. Attach
      // an attribute to recognise unspecified vectors in
      // `base_c_invoke()`.
      if (opts->fallback.s3 && vec_is_common_class_fallback(to_col)) {
        PROTECT(col);
        Rf_setAttrib(col, Rf_install("vctrs:::unspecified"), vctrs_shared_true);
        UNPROTECT(1);
      }
    } else {
      --pos; // 1-based index
      ++common_len;
      x_arg_loc = pos;

      struct cast_opts col_opts = {
        .x = VECTOR_ELT(x, pos),
        .to = VECTOR_ELT(to, i),
        .p_x_arg = named_x_arg,
        .p_to_arg = named_to_arg,
        .call = opts->call,
        .fallback = opts->fallback
      };
      col = vec_cast_opts(&col_opts);
    }

    SET_VECTOR_ELT(out, i, col);
  }

  // Restore data frame size before calling `vec_restore()`. `x` and
  // `to` might not have any columns to compute the original size.
  init_data_frame(out, size);
  Rf_setAttrib(out, R_RowNamesSymbol, df_rownames(x));

  R_len_t extra_len = Rf_length(x) - common_len;
  if (extra_len) {
    out = vctrs_dispatch3(syms_df_lossy_cast, fns_df_lossy_cast,
                          syms_out, out,
                          syms_x, x,
                          syms_to, to);
  }

  UNPROTECT(4);
  return out;
}

static SEXP df_cast_loop(const struct cast_opts* opts, SEXP names) {
  SEXP x = opts->x;
  SEXP to = opts->to;

  R_len_t len = Rf_length(names);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, len));
  Rf_setAttrib(out, R_NamesSymbol, names);

  R_len_t size = df_size(x);

  r_ssize i = 0;
  struct vctrs_arg* named_x_arg = new_subscript_arg(opts->p_x_arg, names, len, &i);
  KEEP(named_x_arg->shelter);

  struct vctrs_arg* named_to_arg = new_subscript_arg(opts->p_to_arg, names, len, &i);
  KEEP(named_to_arg->shelter);

  for (; i < len; ++i) {
    struct cast_opts col_opts = {
      .x = VECTOR_ELT(x, i),
      .to = VECTOR_ELT(to, i),
      .p_x_arg = named_x_arg,
      .p_to_arg = named_to_arg,
      .call = opts->call,
      .fallback = opts->fallback
    };
    SEXP col = vec_cast_opts(&col_opts);

    SET_VECTOR_ELT(out, i, col);
  }

  // Restore data frame size before calling `vec_restore()`. `x` and
  // `to` might not have any columns to compute the original size.
  init_data_frame(out, size);
  Rf_setAttrib(out, R_RowNamesSymbol, df_rownames(x));

  UNPROTECT(3);
  return out;
}

// If negative index, value is appended
SEXP df_poke(SEXP x, R_len_t i, SEXP value) {
  if (i >= 0) {
    SET_VECTOR_ELT(x, i, value);
    return x;
  }

  R_len_t ncol = Rf_length(x);

  SEXP tmp = PROTECT(r_resize(x, ncol + 1));
  Rf_copyMostAttrib(x, tmp);
  x = tmp;

  SET_VECTOR_ELT(x, ncol, value);

  UNPROTECT(1);
  return x;
}
SEXP df_poke_at(SEXP x, SEXP name, SEXP value) {
  SEXP names = PROTECT(r_names(x));
  R_len_t i = r_chr_find(names, name);
  UNPROTECT(1);

  x = PROTECT(df_poke(x, i, value));

  if (i < 0) {
    SEXP names = PROTECT(r_names(x));
    SET_STRING_ELT(names, Rf_length(x) - 1, name);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return x;
}

static inline
R_len_t df_flat_width(SEXP x) {
  R_len_t n = Rf_length(x);
  R_len_t out = n;

  const SEXP* v_x = VECTOR_PTR_RO(x);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = v_x[i];
    if (is_data_frame(col)) {
      out = out + df_flat_width(col) - 1;
    }
  }

  return out;
}

struct flatten_info {
  bool flatten;
  R_len_t width;
};

static inline
struct flatten_info df_flatten_info(SEXP x) {
  bool flatten = false;

  R_len_t n = Rf_length(x);
  R_len_t width = n;

  const SEXP* v_x = VECTOR_PTR_RO(x);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = v_x[i];
    if (is_data_frame(col)) {
      flatten = true;
      width = width + df_flat_width(col) - 1;
    }
  }

  return (struct flatten_info){flatten, width};
}

// [[ register() ]]
SEXP vctrs_df_flatten_info(SEXP x) {
  struct flatten_info info = df_flatten_info(x);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, r_lgl(info.flatten));
  SET_VECTOR_ELT(out, 1, r_int(info.width));

  UNPROTECT(1);
  return out;
}

static R_len_t df_flatten_loop(SEXP x, SEXP out, SEXP out_names, R_len_t counter);

// Might return duplicate names. Currently only used for equality
// proxy so this doesn't matter. A less bare bone version would repair
// names.
//
// [[ register(); include("type-data-frame.h") ]]
SEXP df_flatten(SEXP x) {
  struct flatten_info info = df_flatten_info(x);

  if (!info.flatten) {
    return x;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, info.width));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, info.width));
  r_attrib_poke_names(out, out_names);

  df_flatten_loop(x, out, out_names, 0);
  init_data_frame(out, df_size(x));

  UNPROTECT(2);
  return out;
}

static R_len_t df_flatten_loop(SEXP x, SEXP out, SEXP out_names, R_len_t counter) {
  R_len_t n = Rf_length(x);
  SEXP x_names = PROTECT(r_names(x));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = VECTOR_ELT(x, i);

    if (is_data_frame(col)) {
      counter = df_flatten_loop(col, out, out_names, counter);
    } else {
      SET_VECTOR_ELT(out, counter, col);
      SET_STRING_ELT(out_names, counter, STRING_ELT(x_names, i));
      ++counter;
    }
  }

  UNPROTECT(1);
  return counter;
}

SEXP df_repair_names(SEXP x, struct name_repair_opts* name_repair) {
  SEXP nms = PROTECT(r_names(x));
  SEXP repaired = PROTECT(vec_as_names(nms, name_repair));

  // Should this go through proxy and restore so that classes can
  // update metadata and check invariants when special columns are
  // renamed?
  if (nms != repaired) {
    x = PROTECT(r_clone_referenced(x));
    r_attrib_poke_names(x, repaired);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return x;
}


void vctrs_init_type_data_frame(SEXP ns) {
  syms_df_lossy_cast = Rf_install("df_lossy_cast");
  fns_df_lossy_cast = Rf_findVar(syms_df_lossy_cast, ns);
}
