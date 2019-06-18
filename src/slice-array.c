#include "vctrs.h"
#include "utils.h"

SEXP vec_strides(const int* p_dim, const R_len_t shape_n) {
  SEXP strides = PROTECT(Rf_allocVector(INTSXP, shape_n));
  int* p_strides = INTEGER(strides);
  int stride = 1;

  for (int i = 0; i < shape_n; ++i) {
    stride *= p_dim[i];
    p_strides[i] = stride;
  }

  UNPROTECT(1);
  return strides;
}

// size_index is R index based, p_shape_index is C index based, but it
// still computes the correct R index location
static int vec_strided_loc(const int size_index,
                           const int* p_shape_index,
                           const int* p_strides,
                           const R_len_t shape_n) {
  int loc = size_index;
  for (R_len_t i = 0; i < shape_n; ++i) {
    loc += p_strides[i] * p_shape_index[i];
  }
  return loc;
}

struct vec_slice_shaped_info {
  const int* p_dim;
  const int* p_strides;
  const int* p_index;
  int* p_shape_index;
  R_len_t dim_n;
  R_len_t shape_n;
  R_len_t index_n;
  R_len_t n_shape_elements;
  SEXP out_dim;
};

#define SLICE_SHAPED(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE) \
                                                                 \
SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));          \
CTYPE* out_data = DEREF(out);                                    \
const CTYPE* x_data = CONST_DEREF(x);                            \
                                                                 \
int out_loc = 0;                                                 \
                                                                 \
for (int i = 0; i < info.n_shape_elements; ++i) {                \
                                                                 \
  /* Add next 1-D slice position */                              \
  for (int j = 0; j < info.index_n; ++j) {                       \
    int size_index = info.p_index[j];                            \
                                                                 \
    if (size_index == NA_INTEGER) {                              \
      out_data[out_loc] = NA_VALUE;                              \
    } else {                                                     \
      int loc = vec_strided_loc(                                 \
        size_index - 1,                                          \
        info.p_shape_index,                                      \
        info.p_strides,                                          \
        info.shape_n                                             \
      );                                                         \
      out_data[out_loc] = x_data[loc];                           \
    }                                                            \
                                                                 \
    out_loc++;                                                   \
  }                                                              \
                                                                 \
  /* Update shape index */                                       \
  for (int j = 0; j < info.shape_n; ++j) {                       \
    info.p_shape_index[j]++;                                     \
    if (info.p_shape_index[j] < info.p_dim[j + 1]) {             \
      break;                                                     \
    }                                                            \
    info.p_shape_index[j] = 0;                                   \
  }                                                              \
}                                                                \
                                                                 \
UNPROTECT(1);                                                    \
return out

static SEXP lgl_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_SHAPED(LGLSXP, int, LOGICAL, LOGICAL_RO, NA_LOGICAL);
}
static SEXP int_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_SHAPED(INTSXP, int, INTEGER, INTEGER_RO, NA_INTEGER);
}
static SEXP dbl_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_SHAPED(REALSXP, double, REAL, REAL_RO, NA_REAL);
}
static SEXP cpl_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_SHAPED(CPLXSXP, Rcomplex, COMPLEX, COMPLEX_RO, vctrs_shared_na_cpl);
}
static SEXP chr_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_SHAPED(STRSXP, SEXP, STRING_PTR, STRING_PTR_RO, NA_STRING);
}
static SEXP raw_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_SHAPED(RAWSXP, Rbyte, RAW, RAW_RO, 0);
}

#undef SLICE_SHAPED

#define SLICE_BARRIER_SHAPED(RTYPE, GET, SET, NA_VALUE)        \
                                                               \
SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));        \
                                                               \
int out_loc = 0;                                               \
                                                               \
for (int i = 0; i < info.n_shape_elements; ++i) {              \
                                                               \
  /* Add next 1-D slice position */                            \
  for (int j = 0; j < info.index_n; ++j) {                     \
    int size_index = info.p_index[j];                          \
                                                               \
    if (size_index == NA_INTEGER) {                            \
      SET(out, out_loc, NA_VALUE);                             \
    } else {                                                   \
      int loc = vec_strided_loc(                               \
        size_index - 1,                                        \
        info.p_shape_index,                                    \
        info.p_strides,                                        \
        info.shape_n                                           \
      );                                                       \
      SEXP elt = GET(x, loc);                                  \
      SET(out, out_loc, elt);                                  \
    }                                                          \
                                                               \
    out_loc++;                                                 \
  }                                                            \
                                                               \
  /* Update shape index */                                     \
  for (int j = 0; j < info.shape_n; ++j) {                     \
    info.p_shape_index[j]++;                                   \
    if (info.p_shape_index[j] < info.p_dim[j + 1]) {           \
      break;                                                   \
    }                                                          \
    info.p_shape_index[j] = 0;                                 \
  }                                                            \
}                                                              \
                                                               \
UNPROTECT(1);                                                  \
return out

static SEXP list_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_BARRIER_SHAPED(VECSXP, VECTOR_ELT, SET_VECTOR_ELT, R_NilValue);
}

#undef SLICE_BARRIER_SHAPED

SEXP vec_slice_shaped_base(enum vctrs_type type,
                           SEXP x,
                           SEXP index,
                           struct vec_slice_shaped_info info) {
  switch (type) {
  case vctrs_type_logical:   return lgl_slice_shaped(x, index, info);
  case vctrs_type_integer:   return int_slice_shaped(x, index, info);
  case vctrs_type_double:    return dbl_slice_shaped(x, index, info);
  case vctrs_type_complex:   return cpl_slice_shaped(x, index, info);
  case vctrs_type_character: return chr_slice_shaped(x, index, info);
  case vctrs_type_raw:       return raw_slice_shaped(x, index, info);
  case vctrs_type_list:      return list_slice_shaped(x, index, info);
  default: Rf_error("Internal error: Non-vector base type `%s` in `vec_slice_base()`",
                    vec_type_as_str(type));
  }
}

SEXP vec_slice_shaped(enum vctrs_type type, SEXP x, SEXP index) {

  SEXP dim = PROTECT(vec_dim(x));

  struct vec_slice_shaped_info info;
  info.p_dim = INTEGER_RO(dim);
  info.p_index = INTEGER_RO(index);
  info.dim_n = Rf_length(dim);
  info.shape_n = info.dim_n - 1;
  info.index_n = Rf_length(index);

  SEXP strides = PROTECT(vec_strides(info.p_dim, info.shape_n));
  info.p_strides = INTEGER_RO(strides);

  info.out_dim = PROTECT(Rf_allocVector(INTSXP, info.dim_n));
  int* p_out_dim = INTEGER(info.out_dim);
  p_out_dim[0] = info.index_n;
  for (int i = 1; i < info.dim_n; ++i) {
    p_out_dim[i] = info.p_dim[i];
  }

  SEXP shape_index = PROTECT(Rf_allocVector(INTSXP, info.shape_n));
  info.p_shape_index = INTEGER(shape_index);
  for (int i = 0; i < info.shape_n; ++i) {
    info.p_shape_index[i] = 0;
  }

  info.n_shape_elements = 1;
  for (int i = 1; i < info.dim_n; ++i) {
    info.n_shape_elements *= info.p_dim[i];
  }

  SEXP out = vec_slice_shaped_base(type, x, index, info);

  UNPROTECT(4);
  return out;
}
