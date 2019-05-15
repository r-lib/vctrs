#include "vctrs.h"
#include "utils.h"

R_len_t rcrd_size(SEXP x);


R_len_t vec_dim(SEXP x) {
  if (has_dim(x)) {
    return Rf_length(x);
  } else {
    return 1;
  }
}

static R_len_t vec_size_impl(SEXP x, bool dispatch) {
  int nprot = 0;

  struct vctrs_proxy_info info = PROTECT_PROXY_INFO(vec_proxy_info(x), &nprot);
  SEXP data = info.proxy;

  R_len_t size;
  switch (info.type) {
  case vctrs_type_null:
    size = 0;
    break;

  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list: {
    SEXP dims = Rf_getAttrib(data, R_DimSymbol);
    if (dims == R_NilValue || Rf_length(dims) == 0) {
      size = Rf_length(data);
      break;
    }

    if (TYPEOF(dims) != INTSXP) {
      Rf_errorcall(R_NilValue, "Corrupt vector: dims is not integer vector");
    }

    size = INTEGER(dims)[0];
    break;
  }

  case vctrs_type_dataframe:
    size = df_size(data);
    break;

  default: {
    struct vctrs_arg_wrapper arg = new_wrapper_arg(NULL, "x");
    stop_scalar_type(x, (struct vctrs_arg*) &arg);
  }}

  UNPROTECT(nprot);
  return size;
}
R_len_t vec_size(SEXP x) {
  return vec_size_impl(x, true);
}

R_len_t df_rownames_size(SEXP x) {
  for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr)) {
    if (TAG(attr) != R_RowNamesSymbol) {
      continue;
    }

    SEXP rn = CAR(attr);
    R_len_t n = Rf_length(rn);

    switch(TYPEOF(rn)) {
    case INTSXP:
      if (is_compact_rownames(rn)) {
        return compact_rownames_length(rn);
      } else {
        return n;
      }
    case STRSXP:
      return n;
    default:
      Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are invalid type");
    }
  }

  return -1;
}

// For performance, avoid Rf_getAttrib() because it automatically transforms
// the rownames into an integer vector
R_len_t df_size(SEXP x) {
  R_len_t n = df_rownames_size(x);

  if (n < 0) {
    Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are missing");
  }

  return n;
}
// Supports bare lists as well
R_len_t df_raw_size(SEXP x) {
  R_len_t n = df_rownames_size(x);
  if (n >= 0) {
    return n;
  }

  if (Rf_length(x) >= 1) {
    return vec_size(VECTOR_ELT(x, 0));
  } else {
    return 0;
  }
}


R_len_t rcrd_size(SEXP x) {
  int n = Rf_length(x);
  if (n == 0) {
    return 0;
  } else {
    return Rf_length(VECTOR_ELT(x, 0));
  }
}

bool has_dim(SEXP x) {
  return ATTRIB(x) != R_NilValue && Rf_getAttrib(x, R_DimSymbol) != R_NilValue;
}

// R interface ------------------------------------------------------------

SEXP vctrs_size(SEXP x) {
  return Rf_ScalarInteger(vec_size(x));
}
