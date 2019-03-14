#ifndef VCTRS_UTILS_H
#define VCTRS_UTILS_H


bool is_bool(SEXP x);

R_len_t r_lgl_sum(SEXP lgl, bool na_true);
SEXP r_lgl_which(SEXP x, bool na_true);

void r_int_fill(SEXP x, int value);

bool r_int_any_na(SEXP x);


#endif
