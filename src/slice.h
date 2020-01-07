#ifndef VCTRS_SLICE_H
#define VCTRS_SLICE_H


SEXP slice_names(SEXP names, SEXP subscript);
SEXP slice_rownames(SEXP names, SEXP subscript);
SEXP vec_slice_base(enum vctrs_type type, SEXP x, SEXP subscript);
SEXP vec_slice_fallback(SEXP x, SEXP subscript);


#endif
