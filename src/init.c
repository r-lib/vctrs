#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP vctrs_list_get(SEXP, SEXP);
extern SEXP vctrs_list_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_field_get(SEXP, SEXP);
extern SEXP vctrs_field_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_fields(SEXP);
extern SEXP vctrs_n_fields(SEXP);
extern SEXP vctrs_hash(SEXP);
extern SEXP vctrs_hash_object(SEXP);
extern SEXP vctrs_equal_object(SEXP);
extern SEXP vctrs_in(SEXP, SEXP);
extern SEXP vctrs_duplicated(SEXP);
extern SEXP vctrs_unique_loc(SEXP);
extern SEXP vctrs_count(SEXP);
extern SEXP vctrs_id(SEXP);
extern SEXP vctrs_n_distinct(SEXP);
extern SEXP vctrs_equal(SEXP, SEXP, SEXP);
extern SEXP vctrs_equal_na(SEXP);
extern SEXP vctrs_compare(SEXP, SEXP, SEXP);
extern SEXP vctrs_match(SEXP, SEXP);
extern SEXP vctrs_duplicated_any(SEXP);
extern SEXP vctrs_size(SEXP);
extern SEXP vctrs_is_unspecified(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"vctrs_list_get",    (DL_FUNC) &vctrs_list_get, 2},
    {"vctrs_list_set",    (DL_FUNC) &vctrs_list_set, 3},
    {"vctrs_field_get",   (DL_FUNC) &vctrs_field_get, 2},
    {"vctrs_field_set",   (DL_FUNC) &vctrs_field_set, 3},
    {"vctrs_fields",      (DL_FUNC) &vctrs_fields,    1},
    {"vctrs_n_fields",    (DL_FUNC) &vctrs_n_fields,  1},
    {"vctrs_hash",        (DL_FUNC) &vctrs_hash,  1},
    {"vctrs_hash_object", (DL_FUNC) &vctrs_hash_object,  1},
    {"vctrs_equal_object", (DL_FUNC) &vctrs_equal_object,  2},
    {"vctrs_in",          (DL_FUNC) &vctrs_in,  2},
    {"vctrs_unique_loc",  (DL_FUNC) &vctrs_unique_loc,  1},
    {"vctrs_duplicated",  (DL_FUNC) &vctrs_duplicated,  1},
    {"vctrs_duplicated_any",  (DL_FUNC) &vctrs_duplicated_any,  1},
    {"vctrs_count",       (DL_FUNC) &vctrs_count,  1},
    {"vctrs_id",          (DL_FUNC) &vctrs_id,  1},
    {"vctrs_n_distinct",  (DL_FUNC) &vctrs_n_distinct,  1},
    {"vctrs_size",      (DL_FUNC) &vctrs_size,  1},
    {"vctrs_is_unspecified",      (DL_FUNC) &vctrs_is_unspecified,  1},
    {"vctrs_equal",       (DL_FUNC) &vctrs_equal,  3},
    {"vctrs_equal_na",       (DL_FUNC) &vctrs_equal_na,  1},
    {"vctrs_compare",     (DL_FUNC) &vctrs_compare,  3},
    {"vctrs_match",       (DL_FUNC) &vctrs_match,  2},
    {NULL, NULL, 0}
};

void R_init_vctrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
