#include "vctrs.h"
#include "altrep-rle.h"
#include <R_ext/Rdynload.h>
#include <stdlib.h> // for NULL
#include <stdbool.h>

// Compile with `-fvisibility=hidden -DHAVE_VISIBILITY_ATTRIBUTE` if you link to this library
#include <R_ext/Visibility.h>
#define export attribute_visible extern


extern SEXP vctrs_list_get(SEXP, SEXP);
extern SEXP vctrs_list_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_field_get(SEXP, SEXP);
extern SEXP vctrs_field_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_fields(SEXP);
extern SEXP vctrs_n_fields(SEXP);
extern SEXP vctrs_hash(SEXP);
extern SEXP vctrs_hash_object(SEXP);
extern SEXP vctrs_equal_object(SEXP, SEXP);
extern SEXP vctrs_duplicated(SEXP);
extern SEXP vctrs_unique_loc(SEXP);
extern SEXP vctrs_count(SEXP);
extern SEXP vctrs_id(SEXP);
extern SEXP vctrs_n_distinct(SEXP);
extern SEXP vec_split(SEXP, SEXP);
extern SEXP vctrs_group_id(SEXP);
extern SEXP vctrs_group_rle(SEXP);
extern SEXP vec_group_loc(SEXP);
extern SEXP vctrs_equal(SEXP, SEXP, SEXP);
extern r_obj* ffi_vec_equal_na(r_obj*);
extern r_obj* ffi_vec_any_missing(r_obj* x);
extern SEXP vctrs_compare(SEXP, SEXP, SEXP);
extern SEXP vctrs_match(SEXP, SEXP, SEXP, SEXP);
extern r_obj* vctrs_in(r_obj*, r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_duplicated_any(SEXP);
extern r_obj* ffi_size(r_obj*, r_obj*);
extern r_obj* ffi_list_sizes(r_obj*, r_obj*);
extern SEXP vctrs_dim(SEXP);
extern SEXP vctrs_dim_n(SEXP);
extern SEXP vctrs_is_unspecified(SEXP);
extern SEXP vctrs_typeof(SEXP, SEXP);
extern SEXP vctrs_is_vector(SEXP);
extern r_obj* ffi_ptype2(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_typeof2(r_obj*, r_obj*);
extern r_obj* ffi_typeof2_s3(r_obj*, r_obj*);
extern r_obj* ffi_cast(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_as_location(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_slice(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_init(r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_chop(SEXP, SEXP);
extern SEXP vctrs_unchop(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_chop_seq(SEXP, SEXP, SEXP, SEXP);
extern r_obj* ffi_slice_seq(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_slice_rep(r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_restore(SEXP, SEXP, SEXP);
extern SEXP vctrs_restore_default(SEXP, SEXP);
extern SEXP vec_proxy(SEXP);
extern SEXP vec_proxy_equal(SEXP);
extern SEXP vec_proxy_compare(SEXP);
extern SEXP vec_proxy_order(SEXP);
extern r_obj* ffi_df_proxy(r_obj*, r_obj*);
extern SEXP vctrs_unspecified(SEXP);
extern r_obj* ffi_ptype(r_obj*, r_obj*, r_obj*);
extern SEXP vec_ptype_finalise(SEXP);
extern r_obj* ffi_minimal_names(r_obj*);
extern r_obj* ffi_unique_names(r_obj*, r_obj*);
extern SEXP ffi_as_minimal_names(SEXP);
extern SEXP vec_names(SEXP);
extern SEXP vctrs_is_unique_names(SEXP);
extern SEXP vctrs_as_unique_names(SEXP, SEXP);
extern SEXP vec_set_names(SEXP, SEXP);
extern r_obj* ffi_df_cast_opts(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_df_ptype2_opts(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_type_info(r_obj*);
extern SEXP ffi_proxy_info(SEXP);
extern r_obj* ffi_class_type(r_obj*);
extern SEXP vctrs_bare_df_restore(SEXP, SEXP, SEXP);
extern r_obj* ffi_recycle(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_assign(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_assign_seq(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_set_attributes(SEXP, SEXP);
extern r_obj* ffi_as_df_row(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_outer_names(r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_df_size(SEXP);
extern r_obj* ffi_as_df_col(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_apply_name_spec(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_unset_s4(r_obj*);
extern SEXP vctrs_validate_name_repair_arg(SEXP);
extern SEXP vctrs_validate_minimal_names(SEXP, SEXP);
extern r_obj* ffi_as_names(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_is_partial(r_obj*);
extern SEXP vctrs_is_list(SEXP);
extern SEXP vctrs_try_catch_callback(SEXP, SEXP);
extern r_obj* ffi_is_coercible(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_as_subscript(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_as_subscript_result(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_df_flatten_info(r_obj*);
extern r_obj* df_flatten(r_obj*);
extern SEXP vctrs_linked_version();
extern r_obj* ffi_tib_ptype2(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_tib_cast(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_assign_params(r_obj*, r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_has_dim(SEXP);
extern SEXP vctrs_rep(SEXP, SEXP);
extern SEXP vctrs_rep_each(SEXP, SEXP);
extern SEXP vctrs_maybe_shared_col(SEXP, SEXP);
extern SEXP vctrs_new_df_unshared_col();
extern SEXP vctrs_shaped_ptype(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_shape2(SEXP, SEXP, SEXP);
extern SEXP vctrs_new_date(SEXP);
extern SEXP vctrs_date_validate(SEXP);
extern SEXP vctrs_new_datetime(SEXP, SEXP);
extern SEXP vctrs_datetime_validate(SEXP);
extern r_obj* ffi_ptype2_opts(r_obj*, r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_s3_find_method(SEXP, SEXP, SEXP);
extern SEXP vctrs_implements_ptype2(SEXP);
extern r_obj* ffi_ptype2_dispatch_native(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_cast_dispatch_native(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_fast_c(SEXP, SEXP);
extern r_obj* ffi_data_frame(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_df_list(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_identify_runs(SEXP);
extern SEXP vctrs_locate_runs(SEXP, SEXP);
extern SEXP vctrs_detect_runs(SEXP, SEXP);
extern SEXP vctrs_slice_complete(SEXP);
extern SEXP vctrs_locate_complete(SEXP);
extern SEXP vctrs_detect_complete(SEXP);
extern SEXP vctrs_normalize_encoding(SEXP);
extern SEXP vctrs_order(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_locate_sorted_groups(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_order_info(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_unrep(SEXP);
extern SEXP vctrs_fill_missing(SEXP, SEXP, SEXP);
extern r_obj* ffi_chr_paste_prefix(r_obj*, r_obj*, r_obj*);
extern r_obj* vctrs_rank(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* vctrs_integer64_proxy(r_obj*);
extern r_obj* vctrs_integer64_restore(r_obj*);
extern r_obj* vctrs_list_drop_empty(r_obj*);
extern r_obj* vctrs_is_altrep(r_obj* x);
extern r_obj* ffi_interleave_indices(r_obj*, r_obj*);
extern r_obj* ffi_compute_nesting_container_info(r_obj*, r_obj*);
extern r_obj* ffi_locate_matches(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_interval_groups(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_interval_locate_groups(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_interval_complement(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_interval_locate_containers(r_obj*, r_obj*);
extern r_obj* ffi_check_list(r_obj*, r_obj*);
extern r_obj* ffi_list_all_vectors(r_obj*, r_obj*);
extern r_obj* ffi_list_check_all_vectors(r_obj*, r_obj*);
extern r_obj* ffi_as_short_length(r_obj*, r_obj*);
extern r_obj* ffi_s3_get_method(r_obj*, r_obj*, r_obj*);


// Maturing
// In the public header
extern bool vec_is_vector(SEXP);
extern R_len_t short_vec_size(SEXP);
extern SEXP short_vec_recycle(SEXP, R_len_t);

// Experimental
// Exported but not available in the public header
extern SEXP exp_vec_cast(SEXP, SEXP);
extern SEXP exp_vec_chop(SEXP, SEXP);
extern SEXP exp_vec_slice_impl(SEXP, SEXP);
extern SEXP exp_vec_names(SEXP);
extern SEXP exp_vec_set_names(SEXP, SEXP);
extern SEXP exp_short_compact_seq(R_len_t, R_len_t, bool);
extern SEXP exp_short_init_compact_seq(int*, R_len_t, R_len_t, bool);

// Defined below
SEXP vctrs_init_library(SEXP);

// Defined in altrep-rle.h
extern SEXP altrep_rle_Make(SEXP);
extern SEXP altrep_rle_is_materialized(SEXP);
void vctrs_init_altrep_rle(DllInfo* dll);

static const R_CallMethodDef CallEntries[] = {
  {"vctrs_list_get",                        (DL_FUNC) &vctrs_list_get, 2},
  {"vctrs_list_set",                        (DL_FUNC) &vctrs_list_set, 3},
  {"vctrs_field_get",                       (DL_FUNC) &vctrs_field_get, 2},
  {"vctrs_field_set",                       (DL_FUNC) &vctrs_field_set, 3},
  {"vctrs_fields",                          (DL_FUNC) &vctrs_fields, 1},
  {"vctrs_n_fields",                        (DL_FUNC) &vctrs_n_fields, 1},
  {"vctrs_hash",                            (DL_FUNC) &vctrs_hash, 1},
  {"vctrs_hash_object",                     (DL_FUNC) &vctrs_hash_object, 1},
  {"vctrs_equal_object",                    (DL_FUNC) &vctrs_equal_object, 2},
  {"vctrs_unique_loc",                      (DL_FUNC) &vctrs_unique_loc, 1},
  {"vctrs_duplicated",                      (DL_FUNC) &vctrs_duplicated, 1},
  {"vctrs_duplicated_any",                  (DL_FUNC) &vctrs_duplicated_any, 1},
  {"vctrs_count",                           (DL_FUNC) &vctrs_count, 1},
  {"vctrs_id",                              (DL_FUNC) &vctrs_id, 1},
  {"vctrs_n_distinct",                      (DL_FUNC) &vctrs_n_distinct, 1},
  {"vctrs_split",                           (DL_FUNC) &vec_split, 2},
  {"vctrs_group_id",                        (DL_FUNC) &vctrs_group_id, 1},
  {"vctrs_group_rle",                       (DL_FUNC) &vctrs_group_rle, 1},
  {"vctrs_group_loc",                       (DL_FUNC) &vec_group_loc, 1},
  {"ffi_size",                              (DL_FUNC) &ffi_size, 2},
  {"ffi_list_sizes",                        (DL_FUNC) &ffi_list_sizes, 2},
  {"vctrs_dim",                             (DL_FUNC) &vctrs_dim, 1},
  {"vctrs_dim_n",                           (DL_FUNC) &vctrs_dim_n, 1},
  {"vctrs_is_unspecified",                  (DL_FUNC) &vctrs_is_unspecified, 1},
  {"vctrs_equal",                           (DL_FUNC) &vctrs_equal, 3},
  {"ffi_vec_equal_na",                      (DL_FUNC) &ffi_vec_equal_na, 1},
  {"ffi_vec_any_missing",                   (DL_FUNC) &ffi_vec_any_missing, 1},
  {"vctrs_compare",                         (DL_FUNC) &vctrs_compare, 3},
  {"vctrs_match",                           (DL_FUNC) &vctrs_match, 4},
  {"vctrs_in",                              (DL_FUNC) &vctrs_in, 4},
  {"vctrs_typeof",                          (DL_FUNC) &vctrs_typeof, 2},
  {"vctrs_init_library",                    (DL_FUNC) &vctrs_init_library, 1},
  {"vctrs_is_vector",                       (DL_FUNC) &vctrs_is_vector, 1},
  {"ffi_ptype2",                            (DL_FUNC) &ffi_ptype2, 3},
  {"ffi_typeof2",                           (DL_FUNC) &ffi_typeof2, 2},
  {"ffi_typeof2_s3",                        (DL_FUNC) &ffi_typeof2_s3, 2},
  {"ffi_cast",                              (DL_FUNC) &ffi_cast, 3},
  {"ffi_as_location",                       (DL_FUNC) &ffi_as_location, 8},
  {"ffi_slice",                             (DL_FUNC) &ffi_slice, 3},
  {"ffi_init",                              (DL_FUNC) &ffi_init, 3},
  {"vctrs_chop",                            (DL_FUNC) &vctrs_chop, 2},
  {"vctrs_unchop",                          (DL_FUNC) &vctrs_unchop, 5},
  {"vctrs_chop_seq",                        (DL_FUNC) &vctrs_chop_seq, 4},
  {"ffi_slice_seq",                         (DL_FUNC) &ffi_slice_seq, 4},
  {"ffi_slice_rep",                         (DL_FUNC) &ffi_slice_rep, 3},
  {"vctrs_restore",                         (DL_FUNC) &vctrs_restore, 3},
  {"vctrs_restore_default",                 (DL_FUNC) &vctrs_restore_default, 2},
  {"vctrs_proxy",                           (DL_FUNC) &vec_proxy, 1},
  {"vctrs_proxy_equal",                     (DL_FUNC) &vec_proxy_equal, 1},
  {"vctrs_proxy_compare",                   (DL_FUNC) &vec_proxy_compare, 1},
  {"vctrs_proxy_order",                     (DL_FUNC) &vec_proxy_order, 1},
  {"ffi_df_proxy",                          (DL_FUNC) &ffi_df_proxy, 2},
  {"vctrs_unspecified",                     (DL_FUNC) &vctrs_unspecified, 1},
  {"ffi_ptype",                             (DL_FUNC) &ffi_ptype, 3},
  {"vctrs_ptype_finalise",                  (DL_FUNC) &vec_ptype_finalise, 1},
  {"ffi_minimal_names",                     (DL_FUNC) &ffi_minimal_names, 1},
  {"ffi_unique_names",                      (DL_FUNC) &ffi_unique_names, 2},
  {"ffi_as_minimal_names",                  (DL_FUNC) &ffi_as_minimal_names, 1},
  {"vctrs_names",                           (DL_FUNC) &vec_names, 1},
  {"vctrs_is_unique_names",                 (DL_FUNC) &vctrs_is_unique_names, 1},
  {"vctrs_as_unique_names",                 (DL_FUNC) &vctrs_as_unique_names, 2},
  {"vctrs_set_names",                       (DL_FUNC) &vec_set_names, 2},
  {"ffi_df_cast_opts",                      (DL_FUNC) &ffi_df_cast_opts, 4},
  {"ffi_df_ptype2_opts",                    (DL_FUNC) &ffi_df_ptype2_opts, 4},
  {"ffi_type_info",                         (DL_FUNC) &ffi_type_info, 1},
  {"ffi_proxy_info",                        (DL_FUNC) &ffi_proxy_info, 1},
  {"ffi_class_type",                        (DL_FUNC) &ffi_class_type, 1},
  {"vctrs_bare_df_restore",                 (DL_FUNC) &vctrs_bare_df_restore, 3},
  {"ffi_recycle",                           (DL_FUNC) &ffi_recycle, 3},
  {"ffi_assign",                            (DL_FUNC) &ffi_assign, 4},
  {"ffi_assign_seq",                        (DL_FUNC) &ffi_assign_seq, 5},
  {"vctrs_set_attributes",                  (DL_FUNC) &vctrs_set_attributes, 2},
  {"ffi_as_df_row",                         (DL_FUNC) &ffi_as_df_row, 3},
  {"ffi_outer_names",                       (DL_FUNC) &ffi_outer_names, 3},
  {"vctrs_df_size",                         (DL_FUNC) &vctrs_df_size, 1},
  {"ffi_as_df_col",                         (DL_FUNC) &ffi_as_df_col, 3},
  {"ffi_apply_name_spec",                   (DL_FUNC) &ffi_apply_name_spec, 4},
  {"ffi_unset_s4",                          (DL_FUNC) &ffi_unset_s4, 1},
  {"vctrs_altrep_rle_Make",                 (DL_FUNC) &altrep_rle_Make, 1},
  {"vctrs_altrep_rle_is_materialized",      (DL_FUNC) &altrep_rle_is_materialized, 1},
  {"vctrs_validate_name_repair_arg",        (DL_FUNC) &vctrs_validate_name_repair_arg, 1},
  {"vctrs_validate_minimal_names",          (DL_FUNC) &vctrs_validate_minimal_names, 2},
  {"ffi_as_names",                          (DL_FUNC) &ffi_as_names, 4},
  {"ffi_is_partial",                        (DL_FUNC) &ffi_is_partial, 1},
  {"vctrs_is_list",                         (DL_FUNC) &vctrs_is_list, 1},
  {"vctrs_try_catch_callback",              (DL_FUNC) &vctrs_try_catch_callback, 2},
  {"ffi_is_coercible",                      (DL_FUNC) &ffi_is_coercible, 4},
  {"ffi_as_subscript",                      (DL_FUNC) &ffi_as_subscript, 5},
  {"ffi_as_subscript_result",               (DL_FUNC) &ffi_as_subscript_result, 5},
  {"ffi_df_flatten_info",                   (DL_FUNC) &ffi_df_flatten_info, 1},
  {"ffi_df_flatten",                        (DL_FUNC) &df_flatten, 1},
  {"vctrs_linked_version",                  (DL_FUNC) &vctrs_linked_version, 0},
  {"ffi_tib_ptype2",                        (DL_FUNC) &ffi_tib_ptype2, 5},
  {"ffi_tib_cast",                          (DL_FUNC) &ffi_tib_cast, 5},
  {"ffi_assign_params",                     (DL_FUNC) &ffi_assign_params, 4},
  {"vctrs_has_dim",                         (DL_FUNC) &vctrs_has_dim, 1},
  {"vctrs_rep",                             (DL_FUNC) &vctrs_rep, 2},
  {"vctrs_rep_each",                        (DL_FUNC) &vctrs_rep_each, 2},
  {"vctrs_maybe_shared_col",                (DL_FUNC) &vctrs_maybe_shared_col, 2},
  {"vctrs_new_df_unshared_col",             (DL_FUNC) &vctrs_new_df_unshared_col, 0},
  {"vctrs_shaped_ptype",                    (DL_FUNC) &vctrs_shaped_ptype, 4},
  {"vctrs_shape2",                          (DL_FUNC) &vctrs_shape2, 3},
  {"vctrs_new_date",                        (DL_FUNC) &vctrs_new_date, 1},
  {"vctrs_date_validate",                   (DL_FUNC) &vctrs_date_validate, 1},
  {"vctrs_new_datetime",                    (DL_FUNC) &vctrs_new_datetime, 2},
  {"vctrs_datetime_validate",               (DL_FUNC) &vctrs_datetime_validate, 1},
  {"ffi_ptype2_opts",                       (DL_FUNC) &ffi_ptype2_opts, 4},
  {"vctrs_s3_find_method",                  (DL_FUNC) &vctrs_s3_find_method, 3},
  {"vctrs_implements_ptype2",               (DL_FUNC) &vctrs_implements_ptype2, 1},
  {"ffi_ptype2_dispatch_native",            (DL_FUNC) &ffi_ptype2_dispatch_native, 4},
  {"ffi_cast_dispatch_native",              (DL_FUNC) &ffi_cast_dispatch_native, 6},
  {"vctrs_fast_c",                          (DL_FUNC) &vctrs_fast_c, 2},
  {"ffi_data_frame",                        (DL_FUNC) &ffi_data_frame, 4},
  {"ffi_df_list",                           (DL_FUNC) &ffi_df_list, 5},
  {"vctrs_identify_runs",                   (DL_FUNC) &vctrs_identify_runs, 1},
  {"vctrs_locate_runs",                     (DL_FUNC) &vctrs_locate_runs, 2},
  {"vctrs_detect_runs",                     (DL_FUNC) &vctrs_detect_runs, 2},
  {"vctrs_slice_complete",                  (DL_FUNC) &vctrs_slice_complete, 1},
  {"vctrs_locate_complete",                 (DL_FUNC) &vctrs_locate_complete, 1},
  {"vctrs_detect_complete",                 (DL_FUNC) &vctrs_detect_complete, 1},
  {"vctrs_normalize_encoding",              (DL_FUNC) &vctrs_normalize_encoding, 1},
  {"vctrs_order",                           (DL_FUNC) &vctrs_order, 5},
  {"vctrs_locate_sorted_groups",            (DL_FUNC) &vctrs_locate_sorted_groups, 5},
  {"vctrs_order_info",                      (DL_FUNC) &vctrs_order_info, 6},
  {"vctrs_unrep",                           (DL_FUNC) &vctrs_unrep, 1},
  {"vctrs_fill_missing",                    (DL_FUNC) &vctrs_fill_missing, 3},
  {"ffi_chr_paste_prefix",                  (DL_FUNC) &ffi_chr_paste_prefix, 3},
  {"vctrs_rank",                            (DL_FUNC) &vctrs_rank, 7},
  {"vctrs_integer64_proxy",                 (DL_FUNC) &vctrs_integer64_proxy, 1},
  {"vctrs_integer64_restore",               (DL_FUNC) &vctrs_integer64_restore, 1},
  {"vctrs_list_drop_empty",                 (DL_FUNC) &vctrs_list_drop_empty, 1},
  {"vctrs_is_altrep",                       (DL_FUNC) &vctrs_is_altrep, 1},
  {"ffi_interleave_indices",                (DL_FUNC) &ffi_interleave_indices, 2},
  {"ffi_compute_nesting_container_info",    (DL_FUNC) &ffi_compute_nesting_container_info, 2},
  {"ffi_locate_matches",                    (DL_FUNC) &ffi_locate_matches, 13},
  {"ffi_interval_groups",                   (DL_FUNC) &ffi_interval_groups, 4},
  {"ffi_interval_locate_groups",            (DL_FUNC) &ffi_interval_locate_groups, 4},
  {"ffi_interval_complement",               (DL_FUNC) &ffi_interval_complement, 4},
  {"ffi_interval_locate_containers",        (DL_FUNC) &ffi_interval_locate_containers, 2},
  {"ffi_check_list",                        (DL_FUNC) &ffi_check_list, 2},
  {"ffi_list_all_vectors",                  (DL_FUNC) &ffi_list_all_vectors, 2},
  {"ffi_list_check_all_vectors",            (DL_FUNC) &ffi_list_check_all_vectors, 2},
  {"ffi_as_short_length",                   (DL_FUNC) &ffi_as_short_length, 2},
  {"ffi_s3_get_method",                     (DL_FUNC) &ffi_s3_get_method, 3},
  {NULL, NULL, 0}
};

extern r_obj* ffi_ptype_common(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_ptype_common_opts(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_size_common(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_recycle_common(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_cast_common(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_cast_common_opts(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_rbind(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_cbind(r_obj*, r_obj*, r_obj*, r_obj*);
extern SEXP vctrs_c(SEXP, SEXP, SEXP, SEXP);
extern r_obj* ffi_new_data_frame(r_obj*);

static
const R_ExternalMethodDef ExtEntries[] = {
  {"ffi_ptype_common",                 (DL_FUNC) &ffi_ptype_common, 1},
  {"ffi_ptype_common_opts",            (DL_FUNC) &ffi_ptype_common_opts, 2},
  {"ffi_size_common",                  (DL_FUNC) &ffi_size_common, 2},
  {"ffi_recycle_common",               (DL_FUNC) &ffi_recycle_common, 1},
  {"ffi_cast_common",                  (DL_FUNC) &ffi_cast_common, 1},
  {"ffi_cast_common_opts",             (DL_FUNC) &ffi_cast_common_opts, 2},
  {"ffi_rbind",                        (DL_FUNC) &ffi_rbind, 4},
  {"ffi_cbind",                        (DL_FUNC) &ffi_cbind, 3},
  {"vctrs_c",                          (DL_FUNC) &vctrs_c, 3},
  {"ffi_new_data_frame",               (DL_FUNC) &ffi_new_data_frame, -1},
  {NULL, NULL, 0}
};

export void R_init_vctrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);

    // Maturing
    // In the public header
    R_RegisterCCallable("vctrs", "vec_is_vector",      (DL_FUNC) &vec_is_vector);
    R_RegisterCCallable("vctrs", "short_vec_size",     (DL_FUNC) &short_vec_size);
    R_RegisterCCallable("vctrs", "short_vec_recycle",  (DL_FUNC) &short_vec_recycle);

    // Experimental
    // Exported but not available in the public header
    R_RegisterCCallable("vctrs", "exp_vec_cast",                (DL_FUNC) &exp_vec_cast);
    R_RegisterCCallable("vctrs", "exp_vec_chop",                (DL_FUNC) &exp_vec_chop);
    R_RegisterCCallable("vctrs", "exp_vec_slice_impl",          (DL_FUNC) &exp_vec_slice_impl);
    R_RegisterCCallable("vctrs", "exp_vec_names",               (DL_FUNC) &exp_vec_names);
    R_RegisterCCallable("vctrs", "exp_vec_set_names",           (DL_FUNC) &exp_vec_set_names);
    R_RegisterCCallable("vctrs", "exp_short_compact_seq",       (DL_FUNC) &exp_short_compact_seq);
    R_RegisterCCallable("vctrs", "exp_short_init_compact_seq",  (DL_FUNC) &exp_short_init_compact_seq);

    // Altrep classes
    vctrs_init_altrep_rle(dll);
}


void vctrs_init_bind(SEXP ns);
void vctrs_init_cast(SEXP ns);
void vctrs_init_data(SEXP ns);
void vctrs_init_dictionary(SEXP ns);
void vctrs_init_interval(r_obj* ns);
void vctrs_init_match(r_obj* ns);
void vctrs_init_names(SEXP ns);
void vctrs_init_proxy_restore(SEXP ns);
void vctrs_init_slice(SEXP ns);
void vctrs_init_slice_assign(SEXP ns);
void vctrs_init_subscript(SEXP ns);
void vctrs_init_subscript_loc(SEXP ns);
void vctrs_init_ptype(r_obj* ns);
void vctrs_init_ptype2(SEXP ns);
void vctrs_init_ptype2_dispatch(SEXP ns);
void vctrs_init_rep(SEXP ns);
void vctrs_init_type_data_frame(SEXP ns);
void vctrs_init_type_date_time(SEXP ns);
void vctrs_init_type_info(SEXP ns);
void vctrs_init_unspecified(SEXP ns);
void vctrs_init_utils(SEXP ns);
void vctrs_init_globals(r_obj* ns);

r_obj* vctrs_init_library(r_obj* ns) {
  r_init_library(ns);

  vctrs_init_bind(ns);
  vctrs_init_cast(ns);
  vctrs_init_data(ns);
  vctrs_init_dictionary(ns);
  vctrs_init_interval(ns);
  vctrs_init_match(ns);
  vctrs_init_names(ns);
  vctrs_init_proxy_restore(ns);
  vctrs_init_slice(ns);
  vctrs_init_slice_assign(ns);
  vctrs_init_subscript(ns);
  vctrs_init_subscript_loc(ns);
  vctrs_init_ptype(ns);
  vctrs_init_ptype2(ns);
  vctrs_init_ptype2_dispatch(ns);
  vctrs_init_rep(ns);
  vctrs_init_type_data_frame(ns);
  vctrs_init_type_date_time(ns);
  vctrs_init_type_info(ns);
  vctrs_init_unspecified(ns);
  vctrs_init_utils(ns);
  vctrs_init_globals(ns);

  return r_null;
}
