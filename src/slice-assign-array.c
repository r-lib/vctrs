#include "vctrs.h"

#define ASSIGN_SHAPED_INDEX(CTYPE, DEREF, CONST_DEREF)        \
  SEXP out = PROTECT(vec_clone_referenced(proxy, ownership)); \
  CTYPE* p_out = DEREF(out);                                  \
                                                              \
  const CTYPE* p_value = CONST_DEREF(value);                  \
  R_len_t k = 0;                                              \
                                                              \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {        \
    R_len_t loc = vec_strided_loc(                            \
      p_info->p_shape_index,                                  \
      p_info->p_strides,                                      \
      p_info->shape_n                                         \
    );                                                        \
                                                              \
    for (R_len_t j = 0; j < p_info->index_n; ++j, ++k) {      \
      const int step = p_info->p_steps[j];                    \
                                                              \
      if (step == NA_INTEGER) {                               \
        continue;                                             \
      }                                                       \
                                                              \
      loc += step;                                            \
                                                              \
      p_out[loc] = p_value[k];                                \
    }                                                         \
                                                              \
    vec_shape_index_increment(p_info);                        \
  }                                                           \
                                                              \
  UNPROTECT(1);                                               \
  return out

#define ASSIGN_SHAPED_COMPACT(CTYPE, DEREF, CONST_DEREF)      \
  SEXP out = PROTECT(vec_clone_referenced(proxy, ownership)); \
  CTYPE* p_out = DEREF(out);                                  \
                                                              \
  const R_len_t start = p_info->p_index[0];                   \
  const R_len_t n = p_info->p_index[1];                       \
  const R_len_t step = p_info->p_index[2];                    \
                                                              \
  const CTYPE* p_value = CONST_DEREF(value);                  \
  R_len_t k = 0;                                              \
                                                              \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {        \
    R_len_t loc = vec_strided_loc(                            \
      p_info->p_shape_index,                                  \
      p_info->p_strides,                                      \
      p_info->shape_n                                         \
    );                                                        \
                                                              \
    loc += start;                                             \
                                                              \
    for (R_len_t j = 0; j < n; ++j, ++k, loc += step) {       \
      p_out[loc] = p_value[k];                                \
    }                                                         \
                                                              \
    vec_shape_index_increment(p_info);                        \
  }                                                           \
                                                              \
  UNPROTECT(1);                                               \
  return out

// -----------------------------------------------------------------------------

#define ASSIGN_SHAPED(CTYPE, DEREF, CONST_DEREF)      \
  if (is_compact_seq(index)) {                        \
    ASSIGN_SHAPED_COMPACT(CTYPE, DEREF, CONST_DEREF); \
  } else {                                            \
    ASSIGN_SHAPED_INDEX(CTYPE, DEREF, CONST_DEREF);   \
  }

static inline SEXP lgl_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                                     const enum vctrs_ownership ownership,
                                     struct strides_info* p_info) {
  ASSIGN_SHAPED(int, LOGICAL, LOGICAL_RO);
}
static inline SEXP int_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                                     const enum vctrs_ownership ownership,
                                     struct strides_info* p_info) {
  ASSIGN_SHAPED(int, INTEGER, INTEGER_RO);
}
static inline SEXP dbl_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                                     const enum vctrs_ownership ownership,
                                     struct strides_info* p_info) {
  ASSIGN_SHAPED(double, REAL, REAL_RO);
}
static inline SEXP cpl_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                                     const enum vctrs_ownership ownership,
                                     struct strides_info* p_info) {
  ASSIGN_SHAPED(Rcomplex, COMPLEX, COMPLEX_RO);
}
static inline SEXP chr_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                                     const enum vctrs_ownership ownership,
                                     struct strides_info* p_info) {
  ASSIGN_SHAPED(SEXP, STRING_PTR, STRING_PTR_RO);
}
static inline SEXP raw_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                                     const enum vctrs_ownership ownership,
                                     struct strides_info* p_info) {
  ASSIGN_SHAPED(Rbyte, RAW, RAW_RO);
}

#undef ASSIGN_SHAPED
#undef ASSIGN_SHAPED_COMPACT
#undef ASSIGN_SHAPED_INDEX

// -----------------------------------------------------------------------------

#define ASSIGN_BARRIER_SHAPED_INDEX(GET, SET)                 \
  SEXP out = PROTECT(vec_clone_referenced(proxy, ownership)); \
                                                              \
  R_len_t k = 0;                                              \
                                                              \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {        \
    R_len_t loc = vec_strided_loc(                            \
      p_info->p_shape_index,                                  \
      p_info->p_strides,                                      \
      p_info->shape_n                                         \
    );                                                        \
                                                              \
    for (R_len_t j = 0; j < p_info->index_n; ++j, ++k) {      \
      const int step = p_info->p_steps[j];                    \
                                                              \
      if (step == NA_INTEGER) {                               \
        continue;                                             \
      }                                                       \
                                                              \
      loc += step;                                            \
                                                              \
      SEXP elt = GET(value, k);                               \
      SET(out, loc, elt);                                     \
    }                                                         \
                                                              \
    vec_shape_index_increment(p_info);                        \
  }                                                           \
                                                              \
  UNPROTECT(1);                                               \
  return out

#define ASSIGN_BARRIER_SHAPED_COMPACT(GET, SET)               \
  SEXP out = PROTECT(vec_clone_referenced(proxy, ownership)); \
                                                              \
  const R_len_t start = p_info->p_index[0];                   \
  const R_len_t n = p_info->p_index[1];                       \
  const R_len_t step = p_info->p_index[2];                    \
                                                              \
  R_len_t k = 0;                                              \
                                                              \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {        \
    R_len_t loc = vec_strided_loc(                            \
      p_info->p_shape_index,                                  \
      p_info->p_strides,                                      \
      p_info->shape_n                                         \
    );                                                        \
                                                              \
    loc += start;                                             \
                                                              \
    for (R_len_t j = 0; j < n; ++j, ++k, loc += step) {       \
      SEXP elt = GET(value, k);                               \
      SET(out, loc, elt);                                     \
    }                                                         \
                                                              \
    vec_shape_index_increment(p_info);                        \
  }                                                           \
                                                              \
  UNPROTECT(1);                                               \
  return out

// -----------------------------------------------------------------------------

#define ASSIGN_BARRIER_SHAPED(GET, SET)      \
  if (is_compact_seq(index)) {               \
    ASSIGN_BARRIER_SHAPED_COMPACT(GET, SET); \
  } else {                                   \
    ASSIGN_BARRIER_SHAPED_INDEX(GET, SET);   \
  }

static SEXP list_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  const enum vctrs_ownership ownership,
  struct strides_info* p_info
) {
  ASSIGN_BARRIER_SHAPED(VECTOR_ELT, SET_VECTOR_ELT);
}

#undef ASSIGN_BARRIER_SHAPED
#undef ASSIGN_BARRIER_SHAPED_COMPACT
#undef ASSIGN_BARRIER_SHAPED_INDEX

// -----------------------------------------------------------------------------

static inline SEXP vec_assign_shaped_switch(SEXP proxy,
                                            SEXP index,
                                            SEXP value,
                                            const enum vctrs_ownership ownership,
                                            struct strides_info* p_info) {
  switch (vec_proxy_typeof(proxy)) {
  case VCTRS_TYPE_logical:   return lgl_assign_shaped(proxy, index, value, ownership, p_info);
  case VCTRS_TYPE_integer:   return int_assign_shaped(proxy, index, value, ownership, p_info);
  case VCTRS_TYPE_double:    return dbl_assign_shaped(proxy, index, value, ownership, p_info);
  case VCTRS_TYPE_complex:   return cpl_assign_shaped(proxy, index, value, ownership, p_info);
  case VCTRS_TYPE_character: return chr_assign_shaped(proxy, index, value, ownership, p_info);
  case VCTRS_TYPE_raw:       return raw_assign_shaped(proxy, index, value, ownership, p_info);
  case VCTRS_TYPE_list:      return list_assign_shaped(proxy, index, value, ownership, p_info);
  default:                   stop_unimplemented_vctrs_type("vec_assign_shaped_switch",
                                                           vec_proxy_typeof(proxy));
  }
}

// -----------------------------------------------------------------------------

// [[ include("vctrs.h") ]]
SEXP vec_assign_shaped(SEXP proxy, SEXP index, SEXP value, const enum vctrs_ownership ownership) {
  int n_protect = 0;

  struct strides_info info = new_strides_info(proxy, index);
  PROTECT_STRIDES_INFO(&info, &n_protect);

  SEXP out = vec_assign_shaped_switch(proxy, index, value, ownership, &info);

  UNPROTECT(n_protect);
  return out;
}
