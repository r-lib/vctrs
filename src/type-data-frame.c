#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

static SEXP new_compact_rownames(R_len_t n);


// [[ include("type-data-frame.h") ]]
bool is_data_frame(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    return false;
  }

  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble ||
    type == vctrs_class_data_frame;
}

// [[ include("type-data-frame.h") ]]
bool is_native_df(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble;
}

// [[ include("type-data-frame.h") ]]
bool is_bare_data_frame(SEXP x) {
  return class_type(x) == vctrs_class_bare_data_frame;
}

// [[ include("type-data-frame.h") ]]
bool is_bare_tibble(SEXP x) {
  return class_type(x) == vctrs_class_bare_tibble;
}

// [[ include("type-data-frame.h") ]]
SEXP new_data_frame(SEXP x, R_len_t n) {
  x = PROTECT(r_maybe_duplicate(x));
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

  SEXP out = PROTECT(r_maybe_duplicate(x));

  for (SEXP node = attrib; node != R_NilValue; node = CDR(node)) {
    SEXP tag = TAG(node);

    // We might add dynamic dots later on
    if (tag == R_ClassSymbol) {
      Rf_error("Internal error in `new_data_frame()`: Can't supply `class` in `...`.");
    }

    if (tag == R_NamesSymbol) {
      has_names = true;
      continue;
    }

    if (tag == R_RowNamesSymbol) {
      // "row.names" is checked for consistency with n (if provided)
      if (size != rownames_size(CAR(node))) {
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
    return df_raw_size_from_list(x);
  }
  return df_size_from_n(n);
}

static R_len_t df_size_from_n(SEXP n) {
  if (TYPEOF(n) != INTSXP || Rf_length(n) != 1) {
    Rf_errorcall(R_NilValue, "`n` must be an integer of size 1");
  }

  return r_int_get(n, 0);
}

static SEXP c_data_frame_class(SEXP cls) {
  if (TYPEOF(cls) != STRSXP) {
    Rf_errorcall(R_NilValue, "`class` must be NULL or a character vector");
  }
  if (Rf_length(cls) == 0) {
    return classes_data_frame;
  }

  SEXP args = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(args, 0, cls);
  SET_VECTOR_ELT(args, 1, classes_data_frame);

  SEXP out = vec_c(
    args,
    vctrs_shared_empty_chr,
    R_NilValue,
    NULL
  );

  UNPROTECT(1);
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

// [[ include("type-data-frame.h") ]]
SEXP df_rownames(SEXP x) {
  // Required, because getAttrib() already does the transformation to a vector,
  // and getAttrib0() is hidden
  SEXP node = ATTRIB(x);

  while (node != R_NilValue) {
    SEXP tag = TAG(node);

    if (tag == R_RowNamesSymbol) {
      return CAR(node);
    }

    node = CDR(node);
  }

  return R_NilValue;
}

SEXP df_container_type(SEXP x) {
  SEXP type = PROTECT(Rf_allocVector(VECSXP, 0));

  SET_ATTRIB(type, Rf_shallow_duplicate(ATTRIB(x)));
  SET_OBJECT(type, OBJECT(x));
  Rf_setAttrib(type, R_NamesSymbol, vctrs_shared_empty_chr);

  init_compact_rownames(type, df_size(x));

  UNPROTECT(1);
  return type;
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

// [[ include("type-data-frame.h") ]]
R_len_t df_flat_width(SEXP x) {
  R_len_t n = Rf_length(x);
  R_len_t out = n;

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    if (is_data_frame(col)) {
      out = out + df_flat_width(col) - 1;
    }
  }

  return out;
}

// [[ register() ]]
SEXP vctrs_df_flat_width(SEXP x) {
  return r_int(df_flat_width(x));
}


static R_len_t df_flatten_loop(SEXP x, SEXP out, SEXP out_names, R_len_t counter);

// Might return duplicate names. Currently only used for equality
// proxy so this doesn't matter. A less bare bone version would repair
// names.
//
// [[ register(); include("type-data-frame.h") ]]
SEXP df_flatten(SEXP x) {
  R_len_t width = df_flat_width(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, width));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, width));
  r_poke_names(out, out_names);

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
    x = PROTECT(r_maybe_duplicate(x));
    r_poke_names(x, repaired);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return x;
}
