#ifndef VCTRS_H
#define VCTRS_H

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

R_len_t (*vec_size)(SEXP);
SEXP (*vec_proxy)(SEXP);
SEXP (*vec_restore)(SEXP, SEXP, SEXP);
SEXP (*vec_init)(SEXP, R_len_t);
SEXP (*vec_assign_impl)(SEXP, SEXP, SEXP, bool);
SEXP (*vec_slice_impl)(SEXP, SEXP);
SEXP (*vec_names)(SEXP);
SEXP (*vec_set_names)(SEXP, SEXP);

void vctrs_init_api();

#endif
