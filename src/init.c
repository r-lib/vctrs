#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP vctrs_field_get(SEXP, SEXP);
extern SEXP vctrs_field_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_fields(SEXP);
extern SEXP vctrs_n_fields(SEXP);
extern SEXP vctrs_hash(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"vctrs_field_get", (DL_FUNC) &vctrs_field_get, 2},
    {"vctrs_field_set", (DL_FUNC) &vctrs_field_set, 3},
    {"vctrs_fields",    (DL_FUNC) &vctrs_fields,    1},
    {"vctrs_n_fields",  (DL_FUNC) &vctrs_n_fields,  1},
    {"vctrs_hash",      (DL_FUNC) &vctrs_hash,  1},
    {NULL, NULL, 0}
};

void R_init_vctrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
