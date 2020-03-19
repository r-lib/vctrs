#ifndef VCTRS_CAST_H
#define VCTRS_CAST_H


// Defined in cast-bare.c
SEXP int_as_double(SEXP x, bool* lossy);
SEXP lgl_as_double(SEXP x, bool* lossy);
SEXP dbl_as_integer(SEXP x, bool* lossy);
SEXP lgl_as_integer(SEXP x, bool* lossy);
SEXP chr_as_logical(SEXP x, bool* lossy);
SEXP dbl_as_logical(SEXP x, bool* lossy);
SEXP int_as_logical(SEXP x, bool* lossy);


#endif
