#ifndef VCTRS_ALTREP_REP_MACROS_H
#define VCTRS_ALTREP_REP_MACROS_H

#if HAS_ALTREP

// -----------------------------------------------------------------------------
// `new_vctrs_compact_rep_*()`

#define NEW_VCTRS_COMPACT_REP(VALUE, SIZE, VEC) {                                                      \
  SEXP info = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct vctrs_compact_rep_##VEC##_info)));          \
  struct vctrs_compact_rep_##VEC##_info* p_info = (struct vctrs_compact_rep_##VEC##_info*) RAW0(info); \
                                                                                                       \
  p_info->value = VALUE;                                                                               \
  p_info->size = SIZE;                                                                                 \
                                                                                                       \
  SEXP out = R_new_altrep(vctrs_compact_rep_##VEC##_class, info, R_NilValue);                          \
                                                                                                       \
  /* Force duplicate on modify */                                                                      \
  MARK_NOT_MUTABLE(out);                                                                               \
                                                                                                       \
  UNPROTECT(1);                                                                                        \
  return out;                                                                                          \
}

#define VCTRS_NEW_VCTRS_COMPACT_REP(VALUE, SIZE, CTYPE, CONST_DEREF, VEC) { \
  const CTYPE value_ = CONST_DEREF(VALUE)[0];                               \
  R_xlen_t size_ = (R_xlen_t) REAL_RO(SIZE)[0];                             \
                                                                            \
  return new_vctrs_compact_rep_##VEC(value_, size_);                        \
}

// -----------------------------------------------------------------------------
// value / size accessors

#define VCTRS_COMPACT_REP_VALUE(INFO, VEC) (((struct vctrs_compact_rep_##VEC##_info*) RAW0(INFO))->value)
#define VCTRS_COMPACT_REP_SIZE(INFO, VEC) (((struct vctrs_compact_rep_##VEC##_info*) RAW0(INFO))->size)

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_materialize()`

#define VCTRS_COMPACT_REP_MATERIALIZE(X, CTYPE, DEREF, SEXPTYPE, VEC) { \
  const SEXP info = VCTRS_COMPACT_REP_INFO(X);                          \
  const CTYPE value = VCTRS_COMPACT_REP_##VEC##_VALUE(info);            \
  const R_xlen_t size = VCTRS_COMPACT_REP_##VEC##_SIZE(info);           \
                                                                        \
  SEXP out = PROTECT(Rf_allocVector(SEXPTYPE, size));                   \
  CTYPE* p_out = DEREF(out);                                            \
                                                                        \
  for (R_xlen_t i = 0; i < size; ++i) {                                 \
    p_out[i] = value;                                                   \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out;                                                           \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_serialized_state()`

#define VCTRS_COMPACT_REP_SERIALIZED_STATE(X) { \
  return VCTRS_COMPACT_REP_INFO(X);             \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_unserialize()`

#define VCTRS_COMPACT_REP_UNSERIALIZE(CLS, STATE, CTYPE, VEC_UPPER, VEC_LOWER) { \
  const SEXP info = STATE;                                                       \
  const CTYPE value = VCTRS_COMPACT_REP_##VEC_UPPER##_VALUE(info);               \
  const R_xlen_t size = VCTRS_COMPACT_REP_##VEC_UPPER##_SIZE(info);              \
                                                                                 \
  return new_vctrs_compact_rep_##VEC_LOWER(value, size);                         \
}

// -----------------------------------------------------------------------------

#endif

#endif
