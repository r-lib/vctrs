#ifndef VCTRS_C_H
#define VCTRS_C_H


SEXP vec_c_fallback_invoke(SEXP xs, SEXP name_spec);

bool needs_vec_c_homogeneous_fallback(SEXP xs, SEXP ptype);

static inline
bool needs_vec_c_fallback(SEXP ptype) {
  return Rf_inherits(ptype, c_strs_vctrs_common_class_fallback);
}


#endif
