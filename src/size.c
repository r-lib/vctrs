#include <rlang.h>
#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"
#include "slice.h"

static inline R_len_t vec_raw_size(SEXP x) {
  SEXP dimensions = r_dim(x);

  if (dimensions == R_NilValue || Rf_length(dimensions) == 0) {
    return Rf_length(x);
  }

  if (TYPEOF(dimensions) != INTSXP) {
    Rf_errorcall(R_NilValue, "Corrupt vector, `dim` attribute is not an integer vector.");
  }

  R_len_t size = INTEGER(dimensions)[0];

  return size;
}


// [[ include("vctrs.h") ]]
R_len_t vec_size(SEXP x) {
  int nprot = 0;

  struct vctrs_proxy_info info = vec_proxy_info(x);
  PROTECT_PROXY_INFO(&info, &nprot);

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
  case vctrs_type_list:
    size = vec_raw_size(data);
    break;

  case vctrs_type_dataframe:
    size = df_size(data);
    break;

  default:
    stop_scalar_type(x, NULL);
}

  UNPROTECT(nprot);
  return size;
}
// [[ register() ]]
SEXP vctrs_size(SEXP x) {
  return Rf_ScalarInteger(vec_size(x));
}

SEXP list_sizes(SEXP x) {
  if (!vec_is_list(x)) {
    Rf_errorcall(R_NilValue, "`x` must be a list.");
  }

  R_len_t size = vec_size(x);

  const SEXP* v_x = r_list_cbegin(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* v_out = INTEGER(out);

  r_attrib_poke_names(out, vec_names(x));

  for (R_len_t i = 0; i < size; ++i) {
    v_out[i] = vec_size(v_x[i]);
  }

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_list_sizes(SEXP x) {
  return list_sizes(x);
}

R_len_t df_rownames_size(SEXP x) {
  for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr)) {
    if (TAG(attr) != R_RowNamesSymbol) {
      continue;
    }

    return rownames_size(CAR(attr));
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

  return df_raw_size_from_list(x);
}

// [[ include("vctrs.h") ]]
R_len_t df_raw_size_from_list(SEXP x) {
  if (Rf_length(x) >= 1) {
    return vec_size(VECTOR_ELT(x, 0));
  } else {
    return 0;
  }
}

// [[ register() ]]
SEXP vctrs_df_size(SEXP x) {
  return r_int(df_raw_size(x));
}


// [[ include("vctrs.h") ]]
r_obj* vec_recycle2(r_obj* x,
                    r_ssize size,
                    struct vctrs_arg* x_arg,
                    struct r_lazy call) {
  if (x == r_null) {
    return r_null;
  }

  r_ssize n_x = vec_size(x);

  if (n_x == size) {
    return x;
  }

  if (n_x == 1L) {
    r_obj* i = KEEP(compact_rep(1, size));
    r_obj* out = vec_slice_impl(x, i);

    FREE(1);
    return out;
  }

  stop_recycle_incompatible_size(n_x, size, x_arg, call);
}

// [[ register() ]]
r_obj* ffi_recycle(r_obj* x,
                   r_obj* size_obj,
                   r_obj* frame) {
  if (x == r_null || size_obj == r_null) {
    return r_null;
  }

  struct r_lazy recycle_call = { .x = frame, .env = r_null };

  size_obj = KEEP(vec_cast(size_obj,
                           vctrs_shared_empty_int,
                           args_empty,
                           args_empty,
                           recycle_call));
  R_len_t size = r_int_get(size_obj, 0);
  FREE(1);

  struct arg_data_lazy x_arg_ = new_lazy_arg_data(frame, "x_arg");
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);
  struct r_lazy call = { .x = syms_call, .env = frame };

  return vec_recycle2(x, size, &x_arg, call);
}

// [[ include("vctrs.h") ]]
SEXP vec_recycle_fallback(SEXP x, R_len_t size, struct vctrs_arg* x_arg) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  R_len_t x_size = vec_size(x);

  if (x_size == size) {
    return x;
  }

  if (x_size == 1) {
    SEXP subscript = PROTECT(Rf_allocVector(INTSXP, size));
    r_int_fill(subscript, 1, size);

    SEXP out = vec_slice_fallback(x, subscript);

    UNPROTECT(1);
    return out;
  }

  stop_recycle_incompatible_size(x_size, size, x_arg, r_lazy_null);
}


// [[ include("utils.h") ]]
R_len_t size_validate(SEXP size, const char* arg) {
  size = vec_cast(size,
                  vctrs_shared_empty_int,
                  args_empty,
                  args_empty,
                  r_lazy_null);

  if (Rf_length(size) != 1) {
    Rf_errorcall(R_NilValue, "`%s` must be a single integer.", arg);
  }

  int out = r_int_get(size, 0);

  if (out == NA_INTEGER) {
    Rf_errorcall(R_NilValue, "`%s` can't be missing.", arg);
  }

  return out;
}
