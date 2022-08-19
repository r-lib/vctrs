#include "vctrs.h"

#define SLICE_SHAPED_INDEX(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE) \
  SEXP out_dim = PROTECT(Rf_shallow_duplicate(p_info->dim));           \
  INTEGER(out_dim)[0] = p_info->index_n;                               \
                                                                       \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, out_dim));                   \
  CTYPE* out_data = DEREF(out);                                        \
  const CTYPE* x_data = CONST_DEREF(x);                                \
                                                                       \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                 \
    R_len_t loc = vec_strided_loc(                                     \
      p_info->p_shape_index,                                           \
      p_info->p_strides,                                               \
      p_info->shape_n                                                  \
    );                                                                 \
                                                                       \
    for (R_len_t j = 0; j < p_info->index_n; ++j, ++out_data) {        \
      const int step = p_info->p_steps[j];                             \
                                                                       \
      if (step == NA_INTEGER) {                                        \
        *out_data = NA_VALUE;                                          \
        continue;                                                      \
      }                                                                \
                                                                       \
      loc += step;                                                     \
      *out_data = x_data[loc];                                         \
    }                                                                  \
                                                                       \
    vec_shape_index_increment(p_info);                                 \
  }                                                                    \
                                                                       \
  UNPROTECT(2);                                                        \
  return out

#define SLICE_SHAPED_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)   \
  SEXP out_dim = PROTECT(Rf_shallow_duplicate(p_info->dim));                   \
  INTEGER(out_dim)[0] = p_info->index_n;                                       \
                                                                               \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, out_dim));                           \
  CTYPE* out_data = DEREF(out);                                                \
                                                                               \
  int size_index = p_info->p_index[0];                                         \
  if (size_index == NA_INTEGER) {                                              \
    R_len_t out_n = p_info->shape_elem_n * p_info->index_n;                    \
    for (R_len_t i = 0; i < out_n; ++i, ++out_data) {                          \
      *out_data = NA_VALUE;                                                    \
    }                                                                          \
    UNPROTECT(2);                                                              \
    return(out);                                                               \
  }                                                                            \
                                                                               \
  const CTYPE* x_data = CONST_DEREF(x);                                        \
                                                                               \
  /* Convert to C index */                                                     \
  size_index = size_index - 1;                                                 \
                                                                               \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                         \
    R_len_t loc = vec_strided_loc(                                             \
      p_info->p_shape_index,                                                   \
      p_info->p_strides,                                                       \
      p_info->shape_n                                                          \
    );                                                                         \
                                                                               \
    loc += size_index;                                                         \
    const CTYPE elt_x_data = x_data[loc];                                      \
                                                                               \
    for (R_len_t j = 0; j < p_info->index_n; ++j, ++out_data) {                \
      *out_data = elt_x_data;                                                  \
    }                                                                          \
                                                                               \
    vec_shape_index_increment(p_info);                                         \
  }                                                                            \
                                                                               \
  UNPROTECT(2);                                                                \
  return out

#define SLICE_SHAPED_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF) \
  SEXP out_dim = PROTECT(Rf_shallow_duplicate(p_info->dim));       \
  INTEGER(out_dim)[0] = p_info->index_n;                           \
                                                                   \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, out_dim));               \
  CTYPE* out_data = DEREF(out);                                    \
                                                                   \
  R_len_t start = p_info->p_index[0];                              \
  R_len_t n = p_info->p_index[1];                                  \
  R_len_t step = p_info->p_index[2];                               \
                                                                   \
  const CTYPE* x_data = CONST_DEREF(x);                            \
                                                                   \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {             \
    R_len_t loc = vec_strided_loc(                                 \
      p_info->p_shape_index,                                       \
      p_info->p_strides,                                           \
      p_info->shape_n                                              \
    );                                                             \
                                                                   \
    loc += start;                                                  \
                                                                   \
    for (R_len_t j = 0; j < n; ++j, ++out_data, loc += step) {     \
      *out_data = x_data[loc];                                     \
    }                                                              \
                                                                   \
    vec_shape_index_increment(p_info);                             \
  }                                                                \
                                                                   \
  UNPROTECT(2);                                                    \
  return out

#define SLICE_SHAPED(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)          \
  if (is_compact_rep(index)) {                                            \
    SLICE_SHAPED_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE); \
  } else if (is_compact_seq(index)) {                                     \
    SLICE_SHAPED_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF);           \
  } else {                                                                \
    SLICE_SHAPED_INDEX(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE);       \
  }

static SEXP lgl_slice_shaped(SEXP x, SEXP index, struct strides_info* p_info) {
  SLICE_SHAPED(LGLSXP, int, LOGICAL, LOGICAL_RO, NA_LOGICAL);
}
static SEXP int_slice_shaped(SEXP x, SEXP index, struct strides_info* p_info) {
  SLICE_SHAPED(INTSXP, int, INTEGER, INTEGER_RO, NA_INTEGER);
}
static SEXP dbl_slice_shaped(SEXP x, SEXP index, struct strides_info* p_info) {
  SLICE_SHAPED(REALSXP, double, REAL, REAL_RO, NA_REAL);
}
static SEXP cpl_slice_shaped(SEXP x, SEXP index, struct strides_info* p_info) {
  SLICE_SHAPED(CPLXSXP, Rcomplex, COMPLEX, COMPLEX_RO, vctrs_shared_na_cpl);
}
static SEXP chr_slice_shaped(SEXP x, SEXP index, struct strides_info* p_info) {
  SLICE_SHAPED(STRSXP, SEXP, STRING_PTR, STRING_PTR_RO, NA_STRING);
}
static SEXP raw_slice_shaped(SEXP x, SEXP index, struct strides_info* p_info) {
  SLICE_SHAPED(RAWSXP, Rbyte, RAW, RAW_RO, 0);
}

#undef SLICE_SHAPED
#undef SLICE_SHAPED_COMPACT_REP
#undef SLICE_SHAPED_COMPACT_SEQ
#undef SLICE_SHAPED_INDEX

#define SLICE_BARRIER_SHAPED_INDEX(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE) \
  const CTYPE* x_data = CONST_DEREF(x);                                      \
                                                                             \
  SEXP out_dim = PROTECT(Rf_shallow_duplicate(p_info->dim));                 \
  INTEGER(out_dim)[0] = p_info->index_n;                                     \
                                                                             \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, out_dim));                         \
                                                                             \
  R_len_t out_loc = 0;                                                       \
                                                                             \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                       \
    R_len_t loc = vec_strided_loc(                                           \
      p_info->p_shape_index,                                                 \
      p_info->p_strides,                                                     \
      p_info->shape_n                                                        \
    );                                                                       \
                                                                             \
    for (R_len_t j = 0; j < p_info->index_n; ++j, ++out_loc) {               \
      const int step = p_info->p_steps[j];                                   \
                                                                             \
      if (step == NA_INTEGER) {                                              \
        SET(out, out_loc, NA_VALUE);                                         \
        continue;                                                            \
      }                                                                      \
                                                                             \
      loc += step;                                                           \
      SEXP elt = x_data[loc];                                                \
      SET(out, out_loc, elt);                                                \
    }                                                                        \
                                                                             \
    vec_shape_index_increment(p_info);                                       \
  }                                                                          \
                                                                             \
  UNPROTECT(2);                                                              \
  return out

#define SLICE_BARRIER_SHAPED_COMPACT_REP(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE) \
  const CTYPE* x_data = CONST_DEREF(x);                                            \
                                                                                   \
  SEXP out_dim = PROTECT(Rf_shallow_duplicate(p_info->dim));                       \
  INTEGER(out_dim)[0] = p_info->index_n;                                           \
                                                                                   \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, out_dim));                               \
                                                                                   \
  int size_index = p_info->p_index[0];                                             \
  if (size_index == NA_INTEGER) {                                                  \
    R_len_t out_n = p_info->shape_elem_n * p_info->index_n;                        \
    for (R_len_t i = 0; i < out_n; ++i) {                                          \
      SET(out, i, NA_VALUE);                                                       \
    }                                                                              \
    UNPROTECT(2);                                                                  \
    return(out);                                                                   \
  }                                                                                \
                                                                                   \
  R_len_t out_loc = 0;                                                             \
                                                                                   \
  /* Convert to C index */                                                         \
  size_index = size_index - 1;                                                     \
                                                                                   \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                             \
    R_len_t loc = vec_strided_loc(                                                 \
      p_info->p_shape_index,                                                       \
      p_info->p_strides,                                                           \
      p_info->shape_n                                                              \
    );                                                                             \
                                                                                   \
    loc += size_index;                                                             \
    const SEXP elt_x_data = x_data[loc];                                           \
                                                                                   \
    for (R_len_t j = 0; j < p_info->index_n; ++j, ++out_loc) {                     \
      SET(out, out_loc, elt_x_data);                                               \
    }                                                                              \
                                                                                   \
    vec_shape_index_increment(p_info);                                             \
  }                                                                                \
                                                                                   \
  UNPROTECT(2);                                                                    \
  return out

#define SLICE_BARRIER_SHAPED_COMPACT_SEQ(RTYPE, CTYPE, CONST_DEREF, SET) \
  const CTYPE* x_data = CONST_DEREF(x);                                  \
                                                                         \
  SEXP out_dim = PROTECT(Rf_shallow_duplicate(p_info->dim));             \
  INTEGER(out_dim)[0] = p_info->index_n;                                 \
                                                                         \
  SEXP out = PROTECT(Rf_allocArray(RTYPE, out_dim));                     \
                                                                         \
  R_len_t start = p_info->p_index[0];                                    \
  R_len_t n = p_info->p_index[1];                                        \
  R_len_t step = p_info->p_index[2];                                     \
                                                                         \
  R_len_t out_loc = 0;                                                   \
                                                                         \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                   \
    R_len_t loc = vec_strided_loc(                                       \
      p_info->p_shape_index,                                             \
      p_info->p_strides,                                                 \
      p_info->shape_n                                                    \
    );                                                                   \
                                                                         \
    loc += start;                                                        \
                                                                         \
    for (R_len_t j = 0; j < n; ++j, ++out_loc, loc += step) {            \
      SEXP elt = x_data[loc];                                            \
      SET(out, out_loc, elt);                                            \
    }                                                                    \
                                                                         \
    vec_shape_index_increment(p_info);                                   \
  }                                                                      \
                                                                         \
  UNPROTECT(2);                                                          \
  return out

#define SLICE_BARRIER_SHAPED(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE)          \
  if (is_compact_rep(index)) {                                                  \
    SLICE_BARRIER_SHAPED_COMPACT_REP(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE); \
  } else if (is_compact_seq(index)) {                                           \
    SLICE_BARRIER_SHAPED_COMPACT_SEQ(RTYPE, CTYPE, CONST_DEREF, SET);           \
  } else {                                                                      \
    SLICE_BARRIER_SHAPED_INDEX(RTYPE, CTYPE, CONST_DEREF, SET, NA_VALUE);       \
  }

static SEXP list_slice_shaped(SEXP x, SEXP index, struct strides_info* p_info) {
  SLICE_BARRIER_SHAPED(VECSXP, SEXP, VECTOR_PTR_RO, SET_VECTOR_ELT, R_NilValue);
}

#undef SLICE_BARRIER_SHAPED
#undef SLICE_BARRIER_SHAPED_COMPACT_REP
#undef SLICE_BARRIER_SHAPED_COMPACT_SEQ
#undef SLICE_BARRIER_SHAPED_INDEX

SEXP vec_slice_shaped_base(enum vctrs_type type,
                           SEXP x,
                           SEXP index,
                           struct strides_info* p_info) {
  switch (type) {
  case vctrs_type_logical:   return lgl_slice_shaped(x, index, p_info);
  case vctrs_type_integer:   return int_slice_shaped(x, index, p_info);
  case vctrs_type_double:    return dbl_slice_shaped(x, index, p_info);
  case vctrs_type_complex:   return cpl_slice_shaped(x, index, p_info);
  case vctrs_type_character: return chr_slice_shaped(x, index, p_info);
  case vctrs_type_raw:       return raw_slice_shaped(x, index, p_info);
  case vctrs_type_list:      return list_slice_shaped(x, index, p_info);
  default: stop_unimplemented_vctrs_type("vec_slice_shaped_base", type);
  }
}

SEXP vec_slice_shaped(enum vctrs_type type, SEXP x, SEXP index) {
  int n_protect = 0;

  struct strides_info info = new_strides_info(x, index);
  PROTECT_STRIDES_INFO(&info, &n_protect);

  SEXP out = vec_slice_shaped_base(type, x, index, &info);

  UNPROTECT(n_protect);
  return out;
}
