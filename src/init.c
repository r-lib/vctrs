#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP obj_get_(SEXP);
extern SEXP obj_set_(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"obj_get_", (DL_FUNC) &obj_get_, 1},
    {"obj_set_", (DL_FUNC) &obj_set_, 2},
    {NULL, NULL, 0}
};

void R_init_vctrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
