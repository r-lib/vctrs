#include "vctrs.h"

// -----------------------------------------------------------------------------
// Maturing

R_len_t short_vec_size(SEXP x) {
  return vec_size(x);
}

SEXP short_vec_recycle(SEXP x, R_len_t size) {
  return vec_recycle(x, size);
}

// -----------------------------------------------------------------------------
// Experimental

SEXP exp_vec_cast(SEXP x, SEXP to) {
  return vec_cast(x, to, vec_args.empty, vec_args.empty, r_lazy_null);
}

SEXP exp_vec_chop(SEXP x, SEXP indices) {
  return vec_chop_unsafe(x, indices, r_null);
}

SEXP exp_vec_slice_impl(SEXP x, SEXP subscript) {
  return vec_slice_unsafe(x, subscript);
}

SEXP exp_vec_names(SEXP x) {
  return vec_names(x);
}

SEXP exp_vec_set_names(SEXP x, SEXP names) {
  return vec_set_names(x, names);
}

SEXP exp_short_compact_seq(R_len_t start, R_len_t size, bool increasing) {
  return compact_seq(start, size, increasing);
}

void exp_short_init_compact_seq(int* p, R_len_t start, R_len_t size, bool increasing) {
  init_compact_seq(p, start, size, increasing);
}
