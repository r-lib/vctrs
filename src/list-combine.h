#ifndef VCTRS_LIST_COMBINE_H
#define VCTRS_LIST_COMBINE_H

#include "vctrs-core.h"
#include "names.h"
#include "slice-assign.h"

enum list_combine_unmatched {
    LIST_COMBINE_UNMATCHED_default = 0,
    LIST_COMBINE_UNMATCHED_error = 1,
};

r_obj* list_combine(
  r_obj* xs,
  r_obj* indices,
  r_ssize size,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_indices_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
);

// For `list_unchop()`
r_obj* list_combine_for_list_unchop(
  r_obj* xs,
  r_obj* indices,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

// For `vec_c()`
r_obj* list_combine_for_vec_c(
  r_obj* xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

enum list_combine_unmatched parse_unmatched(r_obj* unmatched, struct r_lazy error_call);

// TODO: Exposed for `bind.c`. Can we remove?
bool needs_df_list_combine_common_class_fallback(r_obj* x);

// TODO: Exposed for `bind.c`. Can we remove?
void df_list_combine_common_class_fallback(
  r_obj* out,
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_indices_arg,
  struct r_lazy error_call
);

#endif
