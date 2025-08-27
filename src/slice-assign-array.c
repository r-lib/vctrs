#include "vctrs.h"

#define ASSIGN_SHAPED_LOCATION_INDEX(                                       \
  CTYPE,                                                                    \
  DEREF,                                                                    \
  CONST_DEREF,                                                              \
  SLICE_VALUE,                                                              \
  VALUE_LOC_POST_INDEX_INCREMENT,                                           \
  VALUE_LOC_POST_SHAPE_INCREMENT                                            \
)                                                                           \
  int n_protect = 0;                                                        \
                                                                            \
  struct strides_info info = new_strides_info(proxy, index);                \
  struct strides_info* p_info = &info;                                      \
  PROTECT_STRIDES_INFO(p_info, &n_protect);                                 \
                                                                            \
  SEXP out = PROTECT_N(vec_clone_referenced(proxy, ownership), &n_protect); \
  CTYPE* p_out = DEREF(out);                                                \
                                                                            \
  const CTYPE* p_value = CONST_DEREF(value);                                \
                                                                            \
  /* The `value` location used in the `slice_value = FALSE` */              \
  /* and `vec_size(value) == 1` cases. When `slice_value = TRUE` */         \
  /* and we aren't recycling, `value` tracks `x` instead. */                \
  R_len_t value_loc = 0;                                                    \
                                                                            \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                      \
    R_len_t out_loc = vec_strided_loc(                                      \
      p_info->p_shape_index,                                                \
      p_info->p_strides,                                                    \
      p_info->shape_n                                                       \
    );                                                                      \
                                                                            \
    for (R_len_t index_loc = 0; index_loc < p_info->index_n; ++index_loc) { \
      const int step = p_info->p_steps[index_loc];                          \
                                                                            \
      if (step != NA_INTEGER) {                                             \
        out_loc += step;                                                    \
        p_out[out_loc] = p_value[SLICE_VALUE ? out_loc : value_loc];        \
      }                                                                     \
                                                                            \
      value_loc += VALUE_LOC_POST_INDEX_INCREMENT;                          \
    }                                                                       \
                                                                            \
    vec_shape_index_increment(p_info);                                      \
    value_loc += VALUE_LOC_POST_SHAPE_INCREMENT;                            \
  }                                                                         \
                                                                            \
  UNPROTECT(n_protect);                                                     \
  return out

#define ASSIGN_SHAPED_LOCATION_COMPACT(                                     \
  CTYPE,                                                                    \
  DEREF,                                                                    \
  CONST_DEREF,                                                              \
  SLICE_VALUE,                                                              \
  VALUE_LOC_POST_INDEX_INCREMENT,                                           \
  VALUE_LOC_POST_SHAPE_INCREMENT                                            \
)                                                                           \
  int n_protect = 0;                                                        \
                                                                            \
  struct strides_info info = new_strides_info(proxy, index);                \
  struct strides_info* p_info = &info;                                      \
  PROTECT_STRIDES_INFO(p_info, &n_protect);                                 \
                                                                            \
  SEXP out = PROTECT_N(vec_clone_referenced(proxy, ownership), &n_protect); \
  CTYPE* p_out = DEREF(out);                                                \
                                                                            \
  const R_len_t start = p_info->p_index[0];                                 \
  const R_len_t step = p_info->p_index[2];                                  \
                                                                            \
  const CTYPE* p_value = CONST_DEREF(value);                                \
                                                                            \
  R_len_t value_loc = 0;                                                    \
                                                                            \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                      \
    R_len_t out_loc = vec_strided_loc(                                      \
      p_info->p_shape_index,                                                \
      p_info->p_strides,                                                    \
      p_info->shape_n                                                       \
    );                                                                      \
                                                                            \
    out_loc += start;                                                       \
                                                                            \
    for (R_len_t index_loc = 0; index_loc < p_info->index_n; ++index_loc) { \
      p_out[out_loc] = p_value[SLICE_VALUE ? out_loc : value_loc];          \
      out_loc += step;                                                      \
      value_loc += VALUE_LOC_POST_INDEX_INCREMENT;                          \
    }                                                                       \
                                                                            \
    vec_shape_index_increment(p_info);                                      \
    value_loc += VALUE_LOC_POST_SHAPE_INCREMENT;                            \
  }                                                                         \
                                                                            \
  UNPROTECT(n_protect);                                                     \
  return out

// -----------------------------------------------------------------------------

// Strides information is not required here!
#define ASSIGN_SHAPED_CONDITION_INDEX(                                 \
  CTYPE,                                                               \
  DEREF,                                                               \
  CONST_DEREF,                                                         \
  SLICE_VALUE,                                                         \
  VALUE_LOC_POST_INDEX_INCREMENT,                                      \
  VALUE_LOC_POST_SHAPE_INCREMENT                                       \
)                                                                      \
  r_obj* dim = PROTECT(vec_dim(proxy));                                \
  const int* p_dim = INTEGER_RO(dim);                                  \
  R_len_t dim_n = Rf_length(dim);                                      \
  R_len_t shape_elem_n = vec_shape_elem_n(p_dim, dim_n);               \
                                                                       \
  R_len_t index_size = r_length(index);                                \
  const int* p_index = r_int_cbegin(index);                            \
                                                                       \
  SEXP out = PROTECT(vec_clone_referenced(proxy, ownership));          \
  CTYPE* p_out = DEREF(out);                                           \
  R_len_t out_loc = 0;                                                 \
                                                                       \
  const CTYPE* p_value = CONST_DEREF(value);                           \
  R_len_t value_loc = 0;                                               \
                                                                       \
  for (R_len_t i = 0; i < shape_elem_n; ++i) {                         \
    for (R_len_t index_loc = 0; index_loc < index_size; ++index_loc) { \
      const int index_elt = p_index[index_loc];                        \
      if (index_elt == 1) {                                            \
        p_out[out_loc] = p_value[SLICE_VALUE ? out_loc : value_loc];   \
      }                                                                \
      ++out_loc;                                                       \
      value_loc += VALUE_LOC_POST_INDEX_INCREMENT;                     \
    }                                                                  \
    value_loc += VALUE_LOC_POST_SHAPE_INCREMENT;                       \
  }                                                                    \
                                                                       \
  UNPROTECT(2);                                                        \
  return out

// -----------------------------------------------------------------------------

#define ASSIGN_SHAPED_LOCATION(CTYPE, DEREF, CONST_DEREF)                           \
  const r_ssize value_size = vec_size(value);                                       \
  check_assign_sizes(proxy, index, value_size, slice_value, index_style);           \
                                                                                    \
  if (is_compact_seq(index)) {                                                      \
    if (value_size == 1) {                                                          \
      ASSIGN_SHAPED_LOCATION_COMPACT(CTYPE, DEREF, CONST_DEREF, false, 0, 1);       \
    } else if (should_slice_value(slice_value)) {                                   \
      ASSIGN_SHAPED_LOCATION_COMPACT(CTYPE, DEREF, CONST_DEREF, true, 0, 0);        \
    } else {                                                                        \
      ASSIGN_SHAPED_LOCATION_COMPACT(CTYPE, DEREF, CONST_DEREF, false, 1, 0);       \
    }                                                                               \
  } else {                                                                          \
    if (value_size == 1) {                                                          \
      ASSIGN_SHAPED_LOCATION_INDEX(CTYPE, DEREF, CONST_DEREF, false, 0, 1);         \
    } else if (should_slice_value(slice_value)) {                                   \
      ASSIGN_SHAPED_LOCATION_INDEX(CTYPE, DEREF, CONST_DEREF, true, 0, 0);          \
    } else {                                                                        \
      ASSIGN_SHAPED_LOCATION_INDEX(CTYPE, DEREF, CONST_DEREF, false, 1, 0);         \
    }                                                                               \
  }

#define ASSIGN_SHAPED_CONDITION(CTYPE, DEREF, CONST_DEREF)                                \
  const r_ssize value_size = vec_size(value);                                             \
  check_assign_sizes(proxy, index, value_size, slice_value, index_style);                 \
                                                                                          \
  if (is_compact_seq(index)) {                                                            \
    r_stop_internal(                                                                      \
      "Compact sequence `index` are not supported in the condition path."                 \
    );                                                                                    \
  } else {                                                                                \
    if (value_size == 1) {                                                                \
      ASSIGN_SHAPED_CONDITION_INDEX(CTYPE, DEREF, CONST_DEREF, false, 0, 1);              \
    } else if (should_slice_value(slice_value)) {                                         \
      ASSIGN_SHAPED_CONDITION_INDEX(CTYPE, DEREF, CONST_DEREF, true, 0, 0);               \
    } else {                                                                              \
      ASSIGN_SHAPED_CONDITION_INDEX(CTYPE, DEREF, CONST_DEREF, false, index_elt != 0, 0); \
    }                                                                                     \
  }

#define ASSIGN_SHAPED(CTYPE, DEREF, CONST_DEREF)                        \
  switch (index_style) {                                                \
  case VCTRS_INDEX_STYLE_location: {                                    \
    ASSIGN_SHAPED_LOCATION(CTYPE, DEREF, CONST_DEREF);                  \
  }                                                                     \
  case VCTRS_INDEX_STYLE_condition: {                                   \
    ASSIGN_SHAPED_CONDITION(CTYPE, DEREF, CONST_DEREF);                 \
  }                                                                     \
  default: r_stop_unreachable();                                        \
  }

static inline SEXP lgl_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_SHAPED(int, LOGICAL, LOGICAL_RO);
}
static inline SEXP int_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_SHAPED(int, INTEGER, INTEGER_RO);
}
static inline SEXP dbl_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_SHAPED(double, REAL, REAL_RO);
}
static inline SEXP cpl_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_SHAPED(Rcomplex, COMPLEX, COMPLEX_RO);
}
static inline SEXP raw_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_SHAPED(Rbyte, RAW, RAW_RO);
}

#undef ASSIGN_SHAPED
#undef ASSIGN_SHAPED_LOCATION
#undef ASSIGN_SHAPED_LOCATION_COMPACT
#undef ASSIGN_SHAPED_LOCATION_INDEX
#undef ASSIGN_SHAPED_CONDITION
#undef ASSIGN_SHAPED_CONDITION_INDEX

// -----------------------------------------------------------------------------

#define ASSIGN_BARRIER_SHAPED_LOCATION_INDEX(                               \
  CTYPE,                                                                    \
  CONST_DEREF,                                                              \
  SET,                                                                      \
  SLICE_VALUE,                                                              \
  VALUE_LOC_POST_INDEX_INCREMENT,                                           \
  VALUE_LOC_POST_SHAPE_INCREMENT                                            \
)                                                                           \
  int n_protect = 0;                                                        \
                                                                            \
  struct strides_info info = new_strides_info(proxy, index);                \
  struct strides_info* p_info = &info;                                      \
  PROTECT_STRIDES_INFO(p_info, &n_protect);                                 \
                                                                            \
  SEXP out = PROTECT_N(vec_clone_referenced(proxy, ownership), &n_protect); \
                                                                            \
  CTYPE const* p_value = CONST_DEREF(value);                                \
                                                                            \
  R_len_t value_loc = 0;                                                    \
                                                                            \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                      \
    R_len_t out_loc = vec_strided_loc(                                      \
      p_info->p_shape_index,                                                \
      p_info->p_strides,                                                    \
      p_info->shape_n                                                       \
    );                                                                      \
                                                                            \
    for (R_len_t index_loc = 0; index_loc < p_info->index_n; ++index_loc) { \
      const int step = p_info->p_steps[index_loc];                          \
                                                                            \
      if (step != NA_INTEGER) {                                             \
        out_loc += step;                                                    \
        SET(out, out_loc, p_value[SLICE_VALUE ? out_loc : value_loc]);      \
      }                                                                     \
                                                                            \
      value_loc += VALUE_LOC_POST_INDEX_INCREMENT;                          \
    }                                                                       \
                                                                            \
    vec_shape_index_increment(p_info);                                      \
    value_loc += VALUE_LOC_POST_SHAPE_INCREMENT;                            \
  }                                                                         \
                                                                            \
  UNPROTECT(n_protect);                                                     \
  return out

#define ASSIGN_BARRIER_SHAPED_LOCATION_COMPACT(                             \
  CTYPE,                                                                    \
  CONST_DEREF,                                                              \
  SET,                                                                      \
  SLICE_VALUE,                                                              \
  VALUE_LOC_POST_INDEX_INCREMENT,                                           \
  VALUE_LOC_POST_SHAPE_INCREMENT                                            \
)                                                                           \
  int n_protect = 0;                                                        \
                                                                            \
  struct strides_info info = new_strides_info(proxy, index);                \
  struct strides_info* p_info = &info;                                      \
  PROTECT_STRIDES_INFO(p_info, &n_protect);                                 \
                                                                            \
  SEXP out = PROTECT_N(vec_clone_referenced(proxy, ownership), &n_protect); \
                                                                            \
  const R_len_t start = p_info->p_index[0];                                 \
  const R_len_t step = p_info->p_index[2];                                  \
                                                                            \
  CTYPE const* p_value = CONST_DEREF(value);                                \
                                                                            \
  R_len_t value_loc = 0;                                                    \
                                                                            \
  for (R_len_t i = 0; i < p_info->shape_elem_n; ++i) {                      \
    R_len_t out_loc = vec_strided_loc(                                      \
      p_info->p_shape_index,                                                \
      p_info->p_strides,                                                    \
      p_info->shape_n                                                       \
    );                                                                      \
                                                                            \
    out_loc += start;                                                       \
                                                                            \
    for (R_len_t index_loc = 0; index_loc < p_info->index_n; ++index_loc) { \
      SET(out, out_loc, p_value[SLICE_VALUE ? out_loc : value_loc]);        \
      out_loc += step;                                                      \
      value_loc += VALUE_LOC_POST_INDEX_INCREMENT;                          \
    }                                                                       \
                                                                            \
    vec_shape_index_increment(p_info);                                      \
    value_loc += VALUE_LOC_POST_SHAPE_INCREMENT;                            \
  }                                                                         \
                                                                            \
  UNPROTECT(n_protect);                                                     \
  return out

// -----------------------------------------------------------------------------

// Strides information is not required here!
#define ASSIGN_BARRIER_SHAPED_CONDITION_INDEX(                         \
  CTYPE,                                                               \
  CONST_DEREF,                                                         \
  SET,                                                                 \
  SLICE_VALUE,                                                         \
  VALUE_LOC_POST_INDEX_INCREMENT,                                      \
  VALUE_LOC_POST_SHAPE_INCREMENT                                       \
)                                                                      \
  r_obj* dim = PROTECT(vec_dim(proxy));                                \
  const int* p_dim = INTEGER_RO(dim);                                  \
  R_len_t dim_n = Rf_length(dim);                                      \
  R_len_t shape_elem_n = vec_shape_elem_n(p_dim, dim_n);               \
                                                                       \
  R_len_t index_size = r_length(index);                                \
  const int* p_index = r_int_cbegin(index);                            \
                                                                       \
  SEXP out = PROTECT(vec_clone_referenced(proxy, ownership));          \
  R_len_t out_loc = 0;                                                 \
                                                                       \
  CTYPE const* p_value = CONST_DEREF(value);                           \
  R_len_t value_loc = 0;                                               \
                                                                       \
  for (R_len_t i = 0; i < shape_elem_n; ++i) {                         \
    for (R_len_t index_loc = 0; index_loc < index_size; ++index_loc) { \
      const int index_elt = p_index[index_loc];                        \
      if (index_elt == 1) {                                            \
        SET(out, out_loc, p_value[SLICE_VALUE ? out_loc : value_loc]); \
      }                                                                \
      ++out_loc;                                                       \
      value_loc += VALUE_LOC_POST_INDEX_INCREMENT;                     \
    }                                                                  \
    value_loc += VALUE_LOC_POST_SHAPE_INCREMENT;                       \
  }                                                                    \
                                                                       \
  UNPROTECT(2);                                                        \
  return out

// -----------------------------------------------------------------------------

#define ASSIGN_BARRIER_SHAPED_LOCATION(CTYPE, CONST_DEREF, SET)                         \
  const r_ssize value_size = vec_size(value);                                           \
  check_assign_sizes(proxy, index, value_size, slice_value, index_style);               \
                                                                                        \
  if (is_compact_seq(index)) {                                                          \
    if (value_size == 1) {                                                              \
      ASSIGN_BARRIER_SHAPED_LOCATION_COMPACT(CTYPE, CONST_DEREF, SET, false, 0, 1);     \
    } else if (should_slice_value(slice_value)) {                                       \
      ASSIGN_BARRIER_SHAPED_LOCATION_COMPACT(CTYPE, CONST_DEREF, SET, true, 0, 0);      \
    } else {                                                                            \
      ASSIGN_BARRIER_SHAPED_LOCATION_COMPACT(CTYPE, CONST_DEREF, SET, false, 1, 0);     \
    }                                                                                   \
  } else {                                                                              \
    if (value_size == 1) {                                                              \
      ASSIGN_BARRIER_SHAPED_LOCATION_INDEX(CTYPE, CONST_DEREF, SET, false, 0, 1);       \
    } else if (should_slice_value(slice_value)) {                                       \
      ASSIGN_BARRIER_SHAPED_LOCATION_INDEX(CTYPE, CONST_DEREF, SET, true, 0, 0);        \
    } else {                                                                            \
      ASSIGN_BARRIER_SHAPED_LOCATION_INDEX(CTYPE, CONST_DEREF, SET, false, 1, 0);       \
    }                                                                                   \
  }

#define ASSIGN_BARRIER_SHAPED_CONDITION(CTYPE, CONST_DEREF, SET)                                \
  const r_ssize value_size = vec_size(value);                                                   \
  check_assign_sizes(proxy, index, value_size, slice_value, index_style);                       \
                                                                                                \
  if (is_compact_seq(index)) {                                                                  \
    r_stop_internal(                                                                            \
      "Compact sequence `index` are not supported in the condition path."                       \
    );                                                                                          \
  } else {                                                                                      \
    if (value_size == 1) {                                                                      \
      ASSIGN_BARRIER_SHAPED_CONDITION_INDEX(CTYPE, CONST_DEREF, SET, false, 0, 1);              \
    } else if (should_slice_value(slice_value)) {                                               \
      ASSIGN_BARRIER_SHAPED_CONDITION_INDEX(CTYPE, CONST_DEREF, SET, true, 0, 0);               \
    } else {                                                                                    \
      ASSIGN_BARRIER_SHAPED_CONDITION_INDEX(CTYPE, CONST_DEREF, SET, false, index_elt != 0, 0); \
    }                                                                                           \
  }

#define ASSIGN_BARRIER_SHAPED(CTYPE, CONST_DEREF, SET)                  \
  switch (index_style) {                                                \
  case VCTRS_INDEX_STYLE_location: {                                    \
    ASSIGN_BARRIER_SHAPED_LOCATION(CTYPE, CONST_DEREF, SET);            \
  }                                                                     \
  case VCTRS_INDEX_STYLE_condition: {                                   \
    ASSIGN_BARRIER_SHAPED_CONDITION(CTYPE, CONST_DEREF, SET);           \
  }                                                                     \
  default: r_stop_unreachable();                                        \
  }

static inline SEXP chr_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_BARRIER_SHAPED(SEXP, STRING_PTR_RO, SET_STRING_ELT);
}
static SEXP list_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  const enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_BARRIER_SHAPED(SEXP, VECTOR_PTR_RO, SET_VECTOR_ELT);
}

#undef ASSIGN_BARRIER_SHAPED
#undef ASSIGN_BARRIER_SHAPED_LOCATION
#undef ASSIGN_BARRIER_SHAPED_LOCATION_COMPACT
#undef ASSIGN_BARRIER_SHAPED_LOCATION_INDEX
#undef ASSIGN_BARRIER_SHAPED_CONDITION
#undef ASSIGN_BARRIER_SHAPED_CONDITION_INDEX

// -----------------------------------------------------------------------------

// [[ include("vctrs.h") ]]
SEXP vec_assign_shaped(
  SEXP proxy,
  SEXP index,
  SEXP value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  switch (vec_proxy_typeof(proxy)) {
  case VCTRS_TYPE_logical:   return lgl_assign_shaped(proxy, index, value, ownership, slice_value, index_style);
  case VCTRS_TYPE_integer:   return int_assign_shaped(proxy, index, value, ownership, slice_value, index_style);
  case VCTRS_TYPE_double:    return dbl_assign_shaped(proxy, index, value, ownership, slice_value, index_style);
  case VCTRS_TYPE_complex:   return cpl_assign_shaped(proxy, index, value, ownership, slice_value, index_style);
  case VCTRS_TYPE_character: return chr_assign_shaped(proxy, index, value, ownership, slice_value, index_style);
  case VCTRS_TYPE_raw:       return raw_assign_shaped(proxy, index, value, ownership, slice_value, index_style);
  case VCTRS_TYPE_list:      return list_assign_shaped(proxy, index, value, ownership, slice_value, index_style);
  default:                   stop_unimplemented_vctrs_type("vec_assign_shaped", vec_proxy_typeof(proxy));
  }
}
