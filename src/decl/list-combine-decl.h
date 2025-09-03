static
r_obj* list_combine_with_fallback_opts(
  r_obj* xs,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call,
  const struct fallback_opts fallback_opts
);

static
r_obj* list_combine_impl(
  r_obj* xs,
  bool has_indices,
  struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call,
  const struct fallback_opts fallback_opts
);

static
bool needs_list_combine_common_class_fallback(r_obj* ptype);

static
r_obj* list_combine_common_class_fallback(
  r_obj* xs,
  bool has_indices,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

static
bool needs_list_combine_homogeneous_fallback(r_obj* xs, r_obj* ptype);

static
r_obj* list_combine_homogeneous_fallback(
  r_obj* xs,
  bool has_indices,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

static
r_obj* base_list_combine_fallback(
  r_obj* xs,
  bool has_indices,
  const struct list_combine_indices_info* p_indices_info,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

static
r_obj* base_c_invoke(
  r_obj* xs,
  r_obj* name_spec,
  struct r_lazy error_call
);

static
void stop_name_spec_in_fallback(r_obj* xs, struct r_lazy error_call);

static
r_obj* build_fallback_index(r_obj* indices, r_ssize out_size, struct r_lazy error_call);

static
r_obj* vec_recycle_xs_fallback(
  r_obj* xs,
  r_obj* indices,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

static
bool vec_implements_base_c(r_obj* x);

static
bool class_implements_base_c(r_obj* cls);

static
r_ssize compute_out_size_from_indices(r_obj* indices);
