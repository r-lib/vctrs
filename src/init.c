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
extern SEXP vctrs_hash(SEXP, SEXP );
extern SEXP vctrs_hash_object(SEXP);
extern SEXP vctrs_equal_object(SEXP, SEXP, SEXP);
extern SEXP vctrs_in(SEXP, SEXP);
extern SEXP vctrs_duplicated(SEXP);
extern SEXP vctrs_duplicate_split(SEXP);
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
extern SEXP vec_dim(SEXP x);
extern SEXP vctrs_dim_n(SEXP x);
extern SEXP vctrs_is_unspecified(SEXP);
extern SEXP vctrs_typeof(SEXP, SEXP);
extern SEXP vctrs_is_vector(SEXP);
extern SEXP vctrs_type2(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_typeof2(SEXP, SEXP);
extern SEXP vctrs_cast(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_as_index(SEXP, SEXP, SEXP);
extern SEXP vctrs_slice(SEXP, SEXP);
extern SEXP vec_restore(SEXP, SEXP, SEXP);
extern SEXP vec_restore_default(SEXP, SEXP);
extern SEXP vec_proxy(SEXP);
extern SEXP vctrs_unspecified(SEXP);
extern SEXP vec_type(SEXP);
extern SEXP vec_type_finalise(SEXP);
extern SEXP vctrs_minimal_names(SEXP);
extern SEXP vctrs_unique_names(SEXP, SEXP);
extern SEXP vctrs_as_minimal_names(SEXP);
extern SEXP vec_names(SEXP);
extern SEXP vctrs_is_unique_names(SEXP);
extern SEXP vctrs_as_unique_names(SEXP, SEXP);
extern SEXP vctrs_df_as_dataframe(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_type2_df_df(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_type_info(SEXP);
extern SEXP vctrs_proxy_info(SEXP);
extern SEXP vctrs_class_type(SEXP);
extern SEXP vctrs_df_restore(SEXP, SEXP, SEXP);
extern SEXP vctrs_recycle(SEXP, SEXP);
extern SEXP vctrs_coercible_cast(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_assign(SEXP, SEXP, SEXP);
extern SEXP vctrs_set_attributes(SEXP, SEXP);
extern SEXP vctrs_as_df_row(SEXP, SEXP);
extern SEXP vctrs_outer_names(SEXP, SEXP, SEXP);
extern SEXP vctrs_df_size(SEXP);
extern SEXP vctrs_as_df_col(SEXP, SEXP);
extern SEXP vctrs_apply_name_spec(SEXP, SEXP, SEXP, SEXP);

// Defined below
SEXP vctrs_init(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"vctrs_list_get",                   (DL_FUNC) &vctrs_list_get, 2},
  {"vctrs_list_set",                   (DL_FUNC) &vctrs_list_set, 3},
  {"vctrs_field_get",                  (DL_FUNC) &vctrs_field_get, 2},
  {"vctrs_field_set",                  (DL_FUNC) &vctrs_field_set, 3},
  {"vctrs_fields",                     (DL_FUNC) &vctrs_fields, 1},
  {"vctrs_n_fields",                   (DL_FUNC) &vctrs_n_fields, 1},
  {"vctrs_hash",                       (DL_FUNC) &vctrs_hash, 2},
  {"vctrs_hash_object",                (DL_FUNC) &vctrs_hash_object, 1},
  {"vctrs_equal_object",               (DL_FUNC) &vctrs_equal_object, 3},
  {"vctrs_in",                         (DL_FUNC) &vctrs_in, 2},
  {"vctrs_unique_loc",                 (DL_FUNC) &vctrs_unique_loc, 1},
  {"vctrs_duplicated",                 (DL_FUNC) &vctrs_duplicated, 1},
  {"vctrs_duplicate_split",            (DL_FUNC) &vctrs_duplicate_split, 1},
  {"vctrs_duplicated_any",             (DL_FUNC) &vctrs_duplicated_any, 1},
  {"vctrs_count",                      (DL_FUNC) &vctrs_count, 1},
  {"vctrs_id",                         (DL_FUNC) &vctrs_id, 1},
  {"vctrs_n_distinct",                 (DL_FUNC) &vctrs_n_distinct, 1},
  {"vctrs_size",                       (DL_FUNC) &vctrs_size, 1},
  {"vctrs_dim",                        (DL_FUNC) &vec_dim, 1},
  {"vctrs_dim_n",                      (DL_FUNC) &vctrs_dim_n, 1},
  {"vctrs_is_unspecified",             (DL_FUNC) &vctrs_is_unspecified, 1},
  {"vctrs_equal",                      (DL_FUNC) &vctrs_equal, 3},
  {"vctrs_equal_na",                   (DL_FUNC) &vctrs_equal_na, 1},
  {"vctrs_compare",                    (DL_FUNC) &vctrs_compare, 3},
  {"vctrs_match",                      (DL_FUNC) &vctrs_match, 2},
  {"vctrs_typeof",                     (DL_FUNC) &vctrs_typeof, 2},
  {"vctrs_init",                       (DL_FUNC) &vctrs_init, 1},
  {"vctrs_is_vector",                  (DL_FUNC) &vctrs_is_vector, 1},
  {"vctrs_type2",                      (DL_FUNC) &vctrs_type2, 4},
  {"vctrs_typeof2",                    (DL_FUNC) &vctrs_typeof2, 2},
  {"vctrs_cast",                       (DL_FUNC) &vctrs_cast, 4},
  {"vctrs_as_index",                   (DL_FUNC) &vctrs_as_index, 3},
  {"vctrs_slice",                      (DL_FUNC) &vctrs_slice, 2},
  {"vctrs_restore",                    (DL_FUNC) &vec_restore, 3},
  {"vctrs_restore_default",            (DL_FUNC) &vec_restore_default, 2},
  {"vctrs_proxy",                      (DL_FUNC) &vec_proxy, 1},
  {"vctrs_unspecified",                (DL_FUNC) &vctrs_unspecified, 1},
  {"vctrs_type",                       (DL_FUNC) &vec_type, 1},
  {"vctrs_type_finalise",              (DL_FUNC) &vec_type_finalise, 1},
  {"vctrs_minimal_names",              (DL_FUNC) &vctrs_minimal_names, 1},
  {"vctrs_unique_names",               (DL_FUNC) &vctrs_unique_names, 2},
  {"vctrs_as_minimal_names",           (DL_FUNC) &vctrs_as_minimal_names, 1},
  {"vctrs_names",                      (DL_FUNC) &vec_names, 1},
  {"vctrs_is_unique_names",            (DL_FUNC) &vctrs_is_unique_names, 1},
  {"vctrs_as_unique_names",            (DL_FUNC) &vctrs_as_unique_names, 2},
  {"vctrs_df_as_dataframe",            (DL_FUNC) &vctrs_df_as_dataframe, 4},
  {"vctrs_type2_df_df",                (DL_FUNC) &vctrs_type2_df_df, 4},
  {"vctrs_type_info",                  (DL_FUNC) &vctrs_type_info, 1},
  {"vctrs_proxy_info",                 (DL_FUNC) &vctrs_proxy_info, 1},
  {"vctrs_class_type",                 (DL_FUNC) &vctrs_class_type, 1},
  {"vctrs_df_restore",                 (DL_FUNC) &vctrs_df_restore, 3},
  {"vctrs_recycle",                    (DL_FUNC) &vctrs_recycle, 2},
  {"vctrs_coercible_cast",             (DL_FUNC) &vctrs_coercible_cast, 4},
  {"vctrs_assign",                     (DL_FUNC) &vec_assign, 3},
  {"vctrs_set_attributes",             (DL_FUNC) &vctrs_set_attributes, 2},
  {"vctrs_as_df_row",                  (DL_FUNC) &vctrs_as_df_row, 2},
  {"vctrs_outer_names",                (DL_FUNC) &vctrs_outer_names, 3},
  {"vctrs_df_size",                    (DL_FUNC) &vctrs_df_size, 1},
  {"vctrs_as_df_col",                  (DL_FUNC) &vctrs_as_df_col, 2},
  {"vctrs_apply_name_spec",            (DL_FUNC) &vctrs_apply_name_spec, 4},
  {NULL, NULL, 0}
};

extern SEXP vctrs_type_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_size_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cast_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_rbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_c(SEXP, SEXP, SEXP, SEXP);

static const R_ExternalMethodDef ExtEntries[] = {
  {"vctrs_type_common",                (DL_FUNC) &vctrs_type_common, 1},
  {"vctrs_size_common",                (DL_FUNC) &vctrs_size_common, 2},
  {"vctrs_cast_common",                (DL_FUNC) &vctrs_cast_common, 1},
  {"vctrs_rbind",                      (DL_FUNC) &vctrs_rbind, 3},
  {"vctrs_cbind",                      (DL_FUNC) &vctrs_cbind, 3},
  {"vctrs_c",                          (DL_FUNC) &vctrs_c, 3},
  {NULL, NULL, 0}
};

void R_init_vctrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
}


void vctrs_init_cast(SEXP ns);
void vctrs_init_data(SEXP ns);
void vctrs_init_dictionary(SEXP ns);
void vctrs_init_names(SEXP ns);
void vctrs_init_slice(SEXP ns);
void vctrs_init_slice_assign(SEXP ns);
void vctrs_init_type2(SEXP ns);
void vctrs_init_type(SEXP ns);
void vctrs_init_type_info(SEXP ns);
void vctrs_init_unspecified(SEXP ns);
void vctrs_init_utils(SEXP ns);

SEXP vctrs_init(SEXP ns) {
  vctrs_init_cast(ns);
  vctrs_init_data(ns);
  vctrs_init_dictionary(ns);
  vctrs_init_names(ns);
  vctrs_init_slice(ns);
  vctrs_init_slice_assign(ns);
  vctrs_init_type2(ns);
  vctrs_init_type(ns);
  vctrs_init_type_info(ns);
  vctrs_init_unspecified(ns);
  vctrs_init_utils(ns);
  return R_NilValue;
}
