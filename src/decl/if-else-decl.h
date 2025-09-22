static
r_obj* generic_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* ptype,
  r_ssize size,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call
);

static
r_obj* atomic_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* ptype,
  r_ssize size,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call,
  bool has_missing
);

static
r_obj* atomic_if_else_switch(
  enum r_type type,
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_ssize size,
  r_ssize true_size,
  r_ssize false_size,
  r_ssize missing_size,
  r_obj* true_names,
  r_obj* false_names,
  r_obj* missing_names,
  bool has_missing,
  bool has_true_names,
  bool has_false_names,
  bool has_missing_names
);

static
bool ptype_is_atomic(r_obj* ptype);

static
r_obj* ptype_finalize(
  r_obj* ptype,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  bool has_missing,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call
);
