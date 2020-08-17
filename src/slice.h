#ifndef VCTRS_SLICE_H
#define VCTRS_SLICE_H

extern SEXP syms_vec_slice_dispatch_integer64;
extern SEXP fns_vec_slice_dispatch_integer64;

SEXP slice_names(SEXP names, SEXP subscript);
SEXP slice_rownames(SEXP names, SEXP subscript);
SEXP vec_slice_base(enum vctrs_type type, SEXP x, SEXP subscript);
SEXP vec_slice_fallback(SEXP x, SEXP subscript);

bool vec_is_restored(SEXP x, SEXP to);

SEXP vec_chop2(SEXP x);


#endif
