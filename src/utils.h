#ifndef VCTRS_UTILS_H
#define VCTRS_UTILS_H


bool is_bool(SEXP x);

SEXP vctrs_dispatch1(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x);
SEXP vctrs_dispatch2(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y);
SEXP vctrs_dispatch3(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y,
                     SEXP z_sym, SEXP z);

bool is_compact_rownames(SEXP x);
R_len_t compact_rownames_length(SEXP x);

R_len_t r_lgl_sum(SEXP lgl, bool na_true);
SEXP r_lgl_which(SEXP x, bool na_true);

void r_lgl_fill(SEXP x, int value);
void r_int_fill(SEXP x, int value);

void r_int_fill_seq(SEXP x, int start);

bool r_int_any_na(SEXP x);

SEXP r_new_environment(SEXP parent, R_len_t size);

SEXP r_protect(SEXP x);


extern SEXP vctrs_ns_env;

extern SEXP vctrs_ns_env;

extern SEXP syms_i;
extern SEXP syms_x;
extern SEXP syms_y;
extern SEXP syms_to;
extern SEXP syms_dots;
extern SEXP syms_bracket;

extern SEXP fns_bracket;
extern SEXP fns_quote;


#endif
