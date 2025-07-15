static
r_obj* list_unchop_fallback(
  r_obj* ptype,
  r_obj* xs,
  r_obj* indices,
  r_obj* default_,
  r_ssize out_size,
  enum list_unchop_unmatched unmatched,
  r_obj* name_spec,
  const struct name_repair_opts* name_repair,
  enum fallback_homogeneous homogenous,
  struct vctrs_arg* p_error_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
);

static
r_obj* list_unchop_c_fallback_invoke(
  r_obj* xs,
  r_obj* default_,
  r_obj* name_spec,
  struct r_lazy error_call
);

static
r_obj* list_unchop_c_fallback(
  r_obj* ptype,
  r_obj* xs,
  r_obj* default_,
  r_obj* name_spec,
  const struct name_repair_opts* name_repair,
  struct vctrs_arg* p_error_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy error_call
);

static
void check_any_unmatched(
  r_obj* indices,
  r_ssize out_size,
  struct r_lazy call
);

static
void stop_unchop_unmatched(r_obj* loc, struct r_lazy call);

static
r_obj* compute_default_index(
  r_obj* indices,
  r_ssize out_size
);

static
struct r_vector_bool* detect_unmatched(
  r_obj* indices,
  r_ssize out_size
);

static
r_obj* slice_default(
  r_obj* default_,
  r_obj* default_index,
  r_ssize out_size
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
  r_obj* default_,
  struct vctrs_arg* p_error_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy call
);
