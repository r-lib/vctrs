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

// -----------------------------------------------------------------------------

#endif

#endif
