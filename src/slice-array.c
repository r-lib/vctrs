#include "vctrs.h"
#include "utils.h"

/*
 * Array slicing works by treating the array as a 1D structure, and transforming
 * the `index` passed from R into a series of 1D indices that are used to
 * extract elements from `x` into the new result.
 *
 * Strides represent the offset between elements in the same dimension. For
 * a (2, 2, 2) array, the strides would be [1, 2, 4]. This means that if you
 * flattened this 3D array to 1D in a column major order, there is 1 space
 * between row elements, 2 spaces between column elements and 4 spaces between
 * elements in the third dimension. In practice, we only need the shape strides
 * since the first stride is always 1, so `vec_strides()` only returns the shape
 * strides. Strides are computed as a cumulative product of the `dim`, with an
 * initial value of `1`, this is what `vec_strides()` does.
 *
 * Using the strides, any array index can be converted to a 1D index.
 * This is what `vec_strided_loc()` does. In a (2, 2, 2) array, to find
 * the location at the index [1, 0, 1] (C-based index, 2nd row, 1st col,
 * 2nd elem in 3rd dim) you compute a sum product between the array index
 * and the strides. So it looks like:
 * loc = 1 * (1) + 0 * (2) + 1 * (4) = 5
 * (loc is a C-based index into `x`)
 * Since the first stride is always one, we leave it off and just do:
 * loc = 1 + 0 * (2) + 1 * (4) = 5
 *
 * Example:
 * x = (3, 3, 2) array
 * vec_slice(x, 2:3)
 *
 * strides = [3, 9] // (shape strides)
 *
 * Indices are C-based
 *
 *         | array index | x index | how?
 * -------------------------------------------------------
 * out[0]  | [1, 0, 0]   | 1       | 1 + 0 * (3) + 0 * (9)
 * out[1]  | [2, 0, 0]   | 2       |
 * out[2]  | [1, 1, 0]   | 4       |
 * ...     | ...         | ...     |
 * out[9]  | [2, 1, 1]   | 14      | 2 + 1 * (3) + 1 * (9)
 * out[10] | [1, 2, 1]   | 16      |
 * out[11] | [2, 2, 1]   | 17      |
 *            ^  ^  ^
 *            |   \/
 *            |    |- shape_index
 *            |- size_index
 */

static SEXP vec_strides(const int* p_dim, const R_len_t shape_n) {
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

// To keep the #define as compact as possible, we use a struct to pass around
// important information.
struct vec_slice_shaped_info {
  const int* p_dim;
  const int* p_strides;
  const int* p_index;
  int* p_shape_index;
  R_len_t dim_n;
  R_len_t shape_n;
  R_len_t index_n;
  R_len_t shape_elem_n;
  SEXP out_dim;
};

#define SLICE_SHAPED_INDEX(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE) \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));              \
  CTYPE* out_data = DEREF(out);                                        \
  const CTYPE* x_data = CONST_DEREF(x);                                \
                                                                       \
  for (int i = 0; i < info.shape_elem_n; ++i) {                        \
                                                                       \
    /* Find and add the next `x` element */                            \
    for (int j = 0; j < info.index_n; ++j, ++out_data) {               \
      int size_index = info.p_index[j];                                \
                                                                       \
      if (size_index == NA_INTEGER) {                                  \
        *out_data = NA_VALUE;                                          \
      } else {                                                         \
        int loc = vec_strided_loc(                                     \
          size_index - 1,                                              \
          info.p_shape_index,                                          \
          info.p_strides,                                              \
          info.shape_n                                                 \
        );                                                             \
        *out_data = x_data[loc];                                       \
      }                                                                \
    }                                                                  \
                                                                       \
    /* Update shape_index */                                           \
    for (int j = 0; j < info.shape_n; ++j) {                           \
      info.p_shape_index[j]++;                                         \
      if (info.p_shape_index[j] < info.p_dim[j + 1]) {                 \
        break;                                                         \
      }                                                                \
      info.p_shape_index[j] = 0;                                       \
    }                                                                  \
  }                                                                    \
                                                                       \
  UNPROTECT(1);                                                        \
  return out

#define SLICE_SHAPED_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)   \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));                      \
  CTYPE* out_data = DEREF(out);                                                \
                                                                               \
  int size_index = info.p_index[0];                                            \
  if (size_index == NA_INTEGER) {                                              \
    R_len_t out_n = info.shape_elem_n * info.index_n;                          \
    for (int i = 0; i < out_n; ++i, ++out_data) {                              \
      *out_data = NA_VALUE;                                                    \
    }                                                                          \
    UNPROTECT(1);                                                              \
    return(out);                                                               \
  }                                                                            \
                                                                               \
  const CTYPE* x_data = CONST_DEREF(x);                                        \
                                                                               \
  /* Convert to C index */                                                     \
  size_index = size_index - 1;                                                 \
                                                                               \
  for (int i = 0; i < info.shape_elem_n; ++i) {                                \
                                                                               \
    /* Find and add the next `x` element */                                    \
    for (int j = 0; j < info.index_n; ++j, ++out_data) {                       \
      int loc = vec_strided_loc(                                               \
        size_index,                                                            \
        info.p_shape_index,                                                    \
        info.p_strides,                                                        \
        info.shape_n                                                           \
      );                                                                       \
      *out_data = x_data[loc];                                                 \
    }                                                                          \
                                                                               \
    /* Update shape_index */                                                   \
    for (int j = 0; j < info.shape_n; ++j) {                                   \
      info.p_shape_index[j]++;                                                 \
      if (info.p_shape_index[j] < info.p_dim[j + 1]) {                         \
        break;                                                                 \
      }                                                                        \
      info.p_shape_index[j] = 0;                                               \
    }                                                                          \
  }                                                                            \
                                                                               \
  UNPROTECT(1);                                                                \
  return out

#define SLICE_SHAPED_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF)                    \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));                             \
  CTYPE* out_data = DEREF(out);                                                       \
                                                                                      \
  R_len_t start = info.p_index[0];                                                    \
  R_len_t n = info.p_index[1];                                                        \
  R_len_t step = info.p_index[2];                                                     \
                                                                                      \
  const CTYPE* x_data = CONST_DEREF(x);                                               \
                                                                                      \
  for (int i = 0; i < info.shape_elem_n; ++i) {                                       \
                                                                                      \
    /* Find and add the next `x` element */                                           \
    for (int j = 0, size_index = start; j < n; ++j, size_index += step, ++out_data) { \
      int loc = vec_strided_loc(                                                      \
        size_index,                                                                   \
        info.p_shape_index,                                                           \
        info.p_strides,                                                               \
        info.shape_n                                                                  \
      );                                                                              \
      *out_data = x_data[loc];                                                        \
    }                                                                                 \
                                                                                      \
    /* Update shape_index */                                                          \
    for (int j = 0; j < info.shape_n; ++j) {                                          \
      info.p_shape_index[j]++;                                                        \
      if (info.p_shape_index[j] < info.p_dim[j + 1]) {                                \
        break;                                                                        \
      }                                                                               \
      info.p_shape_index[j] = 0;                                                      \
    }                                                                                 \
  }                                                                                   \
                                                                                      \
  UNPROTECT(1);                                                                       \
  return out

#define SLICE_SHAPED(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)          \
  if (is_compact_rep(index)) {                                            \
    SLICE_SHAPED_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE); \
  } else if (is_compact_seq(index)) {                                     \
    SLICE_SHAPED_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF);           \
  } else {                                                                \
    SLICE_SHAPED_INDEX(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE);       \
  }

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
#undef SLICE_SHAPED_COMPACT_REP
#undef SLICE_SHAPED_COMPACT_SEQ
#undef SLICE_SHAPED_INDEX

#define SLICE_BARRIER_SHAPED_INDEX(RTYPE, GET, SET, NA_VALUE)  \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));      \
                                                               \
  int out_loc = 0;                                             \
                                                               \
  for (int i = 0; i < info.shape_elem_n; ++i) {                \
                                                               \
    /* Find and add the next `x` element */                    \
    for (int j = 0; j < info.index_n; ++j, ++out_loc) {        \
      int size_index = info.p_index[j];                        \
                                                               \
      if (size_index == NA_INTEGER) {                          \
        SET(out, out_loc, NA_VALUE);                           \
      } else {                                                 \
        int loc = vec_strided_loc(                             \
          size_index - 1,                                      \
          info.p_shape_index,                                  \
          info.p_strides,                                      \
          info.shape_n                                         \
        );                                                     \
        SEXP elt = GET(x, loc);                                \
        SET(out, out_loc, elt);                                \
      }                                                        \
    }                                                          \
                                                               \
    /* Update shape_index */                                   \
    for (int j = 0; j < info.shape_n; ++j) {                   \
      info.p_shape_index[j]++;                                 \
      if (info.p_shape_index[j] < info.p_dim[j + 1]) {         \
        break;                                                 \
      }                                                        \
      info.p_shape_index[j] = 0;                               \
    }                                                          \
  }                                                            \
                                                               \
  UNPROTECT(1);                                                \
  return out

#define SLICE_BARRIER_SHAPED_COMPACT_REP(RTYPE, GET, SET, NA_VALUE)   \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));             \
                                                                      \
  int size_index = info.p_index[0];                                   \
  if (size_index == NA_INTEGER) {                                     \
    R_len_t out_n = info.shape_elem_n * info.index_n;                 \
    for (int i = 0; i < out_n; ++i) {                                 \
      SET(out, i, NA_VALUE);                                          \
    }                                                                 \
    UNPROTECT(1);                                                     \
    return(out);                                                      \
  }                                                                   \
                                                                      \
  int out_loc = 0;                                                    \
                                                                      \
  /* Convert to C index */                                            \
  size_index = size_index - 1;                                        \
                                                                      \
  for (int i = 0; i < info.shape_elem_n; ++i) {                       \
                                                                      \
    /* Find and add the next `x` element */                           \
    for (int j = 0; j < info.index_n; ++j, ++out_loc) {               \
      int loc = vec_strided_loc(                                      \
        size_index,                                                   \
        info.p_shape_index,                                           \
        info.p_strides,                                               \
        info.shape_n                                                  \
      );                                                              \
      SEXP elt = GET(x, loc);                                         \
      SET(out, out_loc, elt);                                         \
    }                                                                 \
                                                                      \
    /* Update shape_index */                                          \
    for (int j = 0; j < info.shape_n; ++j) {                          \
      info.p_shape_index[j]++;                                        \
      if (info.p_shape_index[j] < info.p_dim[j + 1]) {                \
        break;                                                        \
      }                                                               \
      info.p_shape_index[j] = 0;                                      \
    }                                                                 \
  }                                                                   \
                                                                      \
  UNPROTECT(1);                                                       \
  return out

#define SLICE_BARRIER_SHAPED_COMPACT_SEQ(RTYPE, GET, SET)                            \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, info.out_dim));                            \
                                                                                     \
  R_len_t start = info.p_index[0];                                                   \
  R_len_t n = info.p_index[1];                                                       \
  R_len_t step = info.p_index[2];                                                    \
                                                                                     \
  int out_loc = 0;                                                                   \
                                                                                     \
  for (int i = 0; i < info.shape_elem_n; ++i) {                                      \
                                                                                     \
    /* Find and add the next `x` element */                                          \
    for (int j = 0, size_index = start; j < n; ++j, size_index += step, ++out_loc) { \
      int loc = vec_strided_loc(                                                     \
        size_index,                                                                  \
        info.p_shape_index,                                                          \
        info.p_strides,                                                              \
        info.shape_n                                                                 \
      );                                                                             \
      SEXP elt = GET(x, loc);                                                        \
      SET(out, out_loc, elt);                                                        \
    }                                                                                \
                                                                                     \
    /* Update shape_index */                                                         \
    for (int j = 0; j < info.shape_n; ++j) {                                         \
      info.p_shape_index[j]++;                                                       \
      if (info.p_shape_index[j] < info.p_dim[j + 1]) {                               \
        break;                                                                       \
      }                                                                              \
      info.p_shape_index[j] = 0;                                                     \
    }                                                                                \
  }                                                                                  \
                                                                                     \
  UNPROTECT(1);                                                                      \
  return out

#define SLICE_BARRIER_SHAPED(RTYPE, GET, SET, NA_VALUE)          \
  if (is_compact_rep(index)) {                                   \
    SLICE_BARRIER_SHAPED_COMPACT_REP(RTYPE, GET, SET, NA_VALUE); \
  } else if (is_compact_seq(index)) {                            \
    SLICE_BARRIER_SHAPED_COMPACT_SEQ(RTYPE, GET, SET);           \
  } else {                                                       \
    SLICE_BARRIER_SHAPED_INDEX(RTYPE, GET, SET, NA_VALUE);       \
  }

static SEXP list_slice_shaped(SEXP x, SEXP index, struct vec_slice_shaped_info info) {
  SLICE_BARRIER_SHAPED(VECSXP, VECTOR_ELT, SET_VECTOR_ELT, R_NilValue);
}

#undef SLICE_BARRIER_SHAPED
#undef SLICE_BARRIER_SHAPED_COMPACT_REP
#undef SLICE_BARRIER_SHAPED_COMPACT_SEQ
#undef SLICE_BARRIER_SHAPED_INDEX

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
  default: Rf_error("Internal error: Non-vector base type `%s` in `vec_slice_shaped_base()`",
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
  info.index_n = vec_subscript_size(index);

  SEXP strides = PROTECT(vec_strides(info.p_dim, info.shape_n));
  info.p_strides = INTEGER_RO(strides);

  // `out_dim` has the same shape as `x`, with an altered size
  // corresponding to the length of the `index`
  info.out_dim = PROTECT(Rf_shallow_duplicate(dim));
  INTEGER(info.out_dim)[0] = info.index_n;

  // Initialize `shape_index` to 0
  SEXP shape_index = PROTECT(Rf_allocVector(INTSXP, info.shape_n));
  info.p_shape_index = INTEGER(shape_index);
  for (int i = 0; i < info.shape_n; ++i) {
    info.p_shape_index[i] = 0;
  }

  info.shape_elem_n = 1;
  for (int i = 1; i < info.dim_n; ++i) {
    info.shape_elem_n *= info.p_dim[i];
  }

  SEXP out = vec_slice_shaped_base(type, x, index, info);

  UNPROTECT(4);
  return out;
}
