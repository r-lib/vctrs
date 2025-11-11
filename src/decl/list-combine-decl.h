static
r_obj* list_combine_impl(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_indices_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call,
  enum s3_fallback s3_fallback
);

static
bool needs_list_combine_common_class_fallback(r_obj* ptype);

static
r_obj* list_combine_common_class_fallback(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* ptype,
  r_obj* name_spec,
  const struct name_repair_opts* p_name_repair_opts,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_indices_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
);

static
bool needs_list_combine_homogeneous_fallback(
  r_obj* xs,
  bool has_default,
  r_obj* default_,
  r_obj* ptype
);

static
r_obj* list_combine_homogeneous_fallback(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
);

static
bool list_all_have_class(r_obj* xs, r_obj* class);

static
bool obj_has_class(r_obj* x, r_obj* class);

static
r_obj* base_list_combine_fallback(
  r_obj* xs,
  bool has_indices,
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  bool has_default,
  r_obj* default_,
  enum list_combine_multiple multiple,
  enum assignment_slice_value slice_xs,
  r_obj* name_spec,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_default_arg,
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
r_obj* build_fallback_index(r_obj* indices, r_ssize size, struct r_lazy error_call);

static
r_obj* vec_recycle_xs_fallback(
  r_obj* xs,
  r_obj* indices,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

static
r_obj* vec_slice_xs_fallback(
  r_obj* xs,
  r_obj* indices,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call
);

static
r_obj* list_location_to_location_indices(r_obj* indices);
static
r_obj* list_condition_to_location_indices(r_obj* indices);

static
bool vec_implements_base_c(r_obj* x);

static
bool class_implements_base_c(r_obj* cls);

static
enum vctrs_index_style compute_indices_style(r_obj* indices, r_ssize size);

static
void check_any_unmatched(
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size,
  struct r_lazy error_call
);

static
void stop_combine_unmatched(r_obj* loc, struct r_lazy error_call);

static
r_obj* compute_default_index(
  r_obj* indices,
  enum vctrs_index_style indices_style,
  r_ssize size
);

static
r_obj* push_default(
  r_obj* xs,
  r_obj* default_
);

static
r_obj* push_default_index(
  r_obj* indices,
  r_obj* default_index
);

static
r_obj* ptype_common_with_default(
  r_obj* ptype,
  r_obj* xs,
  bool has_default,
  r_obj* default_,
  struct vctrs_arg* p_xs_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call,
  enum s3_fallback s3_fallback
);
