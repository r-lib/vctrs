#ifndef ALTREP_RLE_H
#define ALTREP_RLE_H

#include "altrep.h"

SEXP altrep_rle_Make(SEXP input);
R_xlen_t altrep_rle_Length(SEXP vec);
Rboolean altrep_rle_Inspect(
    SEXP x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(SEXP, int, int, int));
SEXP altrep_rle_string_Elt(SEXP vec, R_xlen_t i);
SEXP altrep_rle_Extract_subset(SEXP x, SEXP indx, SEXP call);
SEXP altrep_rle_string_Materialize(SEXP vec);
void* altrep_rle_Dataptr(SEXP vec, Rboolean writeable);
const void* altrep_rle_Dataptr_or_null(SEXP vec);
void vctrs_init_altrep_rle(DllInfo* dll);

extern R_altrep_class_t altrep_rle_class;

#endif
