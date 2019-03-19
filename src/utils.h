#ifndef VCTRS_UTILS_H
#define VCTRS_UTILS_H


bool is_bool(SEXP x);

SEXP vctrs_dispatch2(SEXP fn, SEXP x_sym, SEXP x, SEXP y_sim, SEXP y, SEXP env);

R_len_t r_lgl_sum(SEXP lgl, bool na_true);
SEXP r_lgl_which(SEXP x, bool na_true);

void r_lgl_fill(SEXP x, int value);
void r_int_fill(SEXP x, int value);

void r_int_fill_seq(SEXP x, int start);

bool r_int_any_na(SEXP x);

SEXP r_new_environment(SEXP parent, R_len_t size);


extern SEXP syms_i;
extern SEXP syms_x;
extern SEXP syms_dots;


#endif
