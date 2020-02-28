#include "vctrs.h"
#include "utils.h"

SEXP exp_vec_cast(SEXP x, SEXP to) {
  return vec_cast(x, to, args_empty, args_empty);
}

bool exp_vec_is_vector(SEXP x) {
  return vec_is_vector(x);
}

R_len_t exp_short_vec_size(SEXP x) {
  return vec_size(x);
}

SEXP exp_short_vec_recycle(SEXP x, R_len_t size) {
  return vec_recycle(x, size, args_empty);
}

SEXP exp_short_vec_init(SEXP x, R_len_t size) {
  return vec_init(x, size);
}

SEXP exp_short_compact_seq(R_len_t start, R_len_t size, bool increasing) {
  return compact_seq(start, size, increasing);
}

void exp_short_init_compact_seq(int* p, R_len_t start, R_len_t size, bool increasing) {
  init_compact_seq(p, start, size, increasing);
}
