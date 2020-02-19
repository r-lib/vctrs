#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"


// [[ include("type-data-frame.h") ]]
bool is_data_frame(SEXP x) {
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
static void poke_data_frame_class(SEXP x, SEXP cls);

// [[ register() ]]
SEXP vctrs_new_data_frame(SEXP args) {
  args = CDR(args);

  SEXP x = CAR(args); args = CDR(args);
  SEXP n = CAR(args); args = CDR(args);
  SEXP cls = CAR(args); args = CDR(args);
  SEXP attrib = args;

  if (TYPEOF(x) != VECSXP) {
    Rf_errorcall(R_NilValue, "`x` must be a list");
  }

  R_len_t size = df_size_from_list(x, n);

  if (attrib != R_NilValue) {
    x = r_maybe_duplicate(x);
    SET_ATTRIB(x, attrib);
  }
  PROTECT(x);

  SEXP out = PROTECT(new_data_frame(x, size));

  if (cls != R_NilValue) {
    poke_data_frame_class(out, cls);
  }

  UNPROTECT(2);
  return out;
}

static R_len_t df_size_from_list(SEXP x, SEXP n) {
  if (n == R_NilValue) {
    return df_raw_size_from_list(x);
  }

  if (TYPEOF(n) != INTSXP || Rf_length(n) != 1) {
    Rf_errorcall(R_NilValue, "`n` must be an integer of size 1");
  }

  return r_int_get(n, 0);
}

static void poke_data_frame_class(SEXP x, SEXP cls) {
  if (cls == R_NilValue) {
    return;
  }
  if (TYPEOF(cls) != STRSXP) {
    Rf_errorcall(R_NilValue, "`class` must be NULL or a character vector");
  }
  if (Rf_length(cls) == 0) {
    return;
  }

  SEXP args = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(args, 0, cls);
  SET_VECTOR_ELT(args, 1, classes_data_frame);

  cls = PROTECT(vec_c(
    args,
    vctrs_shared_empty_chr,
    R_NilValue,
    NULL
  ));

  Rf_setAttrib(x, R_ClassSymbol, cls);

  UNPROTECT(2);
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

// [[ include("type-data-frame.h") ]]
R_len_t compact_rownames_length(SEXP x) {
  return abs(INTEGER(x)[1]);
}

static void init_bare_data_frame(SEXP x, R_len_t n);
static SEXP new_compact_rownames(R_len_t n);

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
