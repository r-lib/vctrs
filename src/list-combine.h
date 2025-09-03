#ifndef VCTRS_LIST_COMBINE_H
#define VCTRS_LIST_COMBINE_H

#include "vctrs-core.h"
#include "names.h"

struct list_combine_indices_info {
  r_obj* indices;
  struct vctrs_arg* p_indices_arg;
};

r_obj* list_combine(
  r_obj* xs,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

// TODO: Exposed for `bind.c`. Can we remove?
bool needs_df_list_combine_common_class_fallback(r_obj* x);

// TODO: Exposed for `bind.c`. Can we remove?
void df_list_combine_common_class_fallback(
  r_obj* out,
  r_obj* xs,
  bool has_indices,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct r_lazy error_call,
  r_ssize n_rows
);

#endif
