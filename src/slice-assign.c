#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_assign_fallback = NULL;
SEXP fns_vec_assign_fallback = NULL;

// Defined in slice.c
SEXP vec_as_index(SEXP i, SEXP x);


static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value);
static SEXP vec_assign_impl(SEXP x, SEXP index, SEXP value, SEXP to, bool dispatch);

SEXP vec_assign(SEXP x, SEXP index, SEXP value) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  struct vctrs_arg_wrapper x_arg = new_wrapper_arg(NULL, "x");
  struct vctrs_arg_wrapper value_arg = new_wrapper_arg(NULL, "value");

  vec_assert(x, (struct vctrs_arg*) &x_arg);

  index = PROTECT(vec_as_index(index, x));

  if (has_dim(x)) {
    SEXP out = vec_assign_fallback(x, index, value);
    UNPROTECT(1);
    return out;
  }

  value = PROTECT(vec_coercible_cast(value, x,
                                     (struct vctrs_arg*) &value_arg,
                                     (struct vctrs_arg*) &x_arg));
  value = PROTECT(vec_proxy(value));
  value = PROTECT(vec_recycle(value, vec_size(index)));

  SEXP out = vec_assign_impl(x, index, value, x, true);

  UNPROTECT(4);
  return out;
}


#define ASSIGN(CTYPE, DEREF, CONST_DEREF)                       \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  const CTYPE* value_data = CONST_DEREF(value);                 \
  SEXP out = PROTECT(Rf_shallow_duplicate(x));                  \
  CTYPE* out_data = DEREF(out);                                 \
                                                                \
  for (R_len_t i = 0; i < n; ++i) {                             \
    int j = index_data[i];                                      \
    if (j != NA_INTEGER) {                                      \
      out_data[j - 1] = value_data[i];                          \
    }                                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

static SEXP lgl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, LOGICAL, LOGICAL_RO);
}
static SEXP int_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, INTEGER, INTEGER_RO);
}
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(double, REAL, REAL_RO);
}
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO);
}
static SEXP chr_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(SEXP, STRING_PTR, STRING_PTR_RO);
}
static SEXP raw_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(Rbyte, RAW, RAW_RO);
}

#undef ASSIGN


#define ASSIGN_BARRIER(GET, SET)                                \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  SEXP out = PROTECT(Rf_shallow_duplicate(x));                  \
                                                                \
  for (R_len_t i = 0; i < n; ++i) {                             \
    int j = index_data[i];                                      \
    if (j != NA_INTEGER) {                                      \
      SET(out, j - 1, GET(value, i));                           \
    }                                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

static SEXP list_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN_BARRIER(VECTOR_ELT, SET_VECTOR_ELT);
}

#undef ASSIGN_BARRIER


static SEXP df_assign(SEXP x, SEXP index, SEXP value, SEXP to) {
  R_len_t n = Rf_length(x);
  SEXP out = PROTECT(Rf_shallow_duplicate(x));

  // FIXME: Should that be restored?
  SEXP nms = Rf_getAttrib(x, R_NamesSymbol);
  Rf_setAttrib(out, R_NamesSymbol, nms);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP out_elt = VECTOR_ELT(x, i);
    SEXP value_elt = VECTOR_ELT(value, i);
    SEXP assigned = vec_assign_impl(out_elt, index, value_elt, out_elt, true);
    SET_VECTOR_ELT(out, i, assigned);
  }

  out = vec_restore(out, to, R_NilValue);

  UNPROTECT(1);
  return out;
}

static SEXP oo_assign(SEXP x, SEXP index, SEXP value, SEXP to) {
  SEXP out = PROTECT(vec_proxy(x));
  out = PROTECT(vec_assign_impl(out, index, value, to, false));
  out = vec_restore(out, to, R_NilValue);

  UNPROTECT(2);
  return out;
}


static SEXP vec_assign_impl(SEXP x, SEXP index, SEXP value, SEXP to, bool dispatch) {
  switch (vec_typeof_impl(x, dispatch)) {
  case vctrs_type_logical:   return lgl_assign(x, index, value);
  case vctrs_type_integer:   return int_assign(x, index, value);
  case vctrs_type_double:    return dbl_assign(x, index, value);
  case vctrs_type_complex:   return cpl_assign(x, index, value);
  case vctrs_type_character: return chr_assign(x, index, value);
  case vctrs_type_raw:       return raw_assign(x, index, value);
  case vctrs_type_list:      return list_assign(x, index, value);
  case vctrs_type_dataframe: return df_assign(x, index, value, to);
  case vctrs_type_s3:        return oo_assign(x, index, value, to);
  case vctrs_type_null:      Rf_error("Internal error in `vec_assign_impl()`: Unexpected NULL.");
  case vctrs_type_scalar:    stop_scalar_type(x, args_empty);
  }
}

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value) {
  return vctrs_dispatch3(syms_vec_assign_fallback, fns_vec_assign_fallback,
                         syms_x, x,
                         syms_i, index,
                         syms_value, value);
}


void vctrs_init_slice_assign(SEXP ns) {
  syms_vec_assign_fallback = Rf_install("vec_assign_fallback");
  fns_vec_assign_fallback = Rf_findVar(syms_vec_assign_fallback, ns);
}
