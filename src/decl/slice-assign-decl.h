static r_obj* syms_vec_assign_fallback;
static r_obj* fns_vec_assign_fallback;

static
r_obj* vec_assign_fallback(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

static
r_obj* vec_proxy_assign_names(
  r_obj* proxy,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

static
r_obj* lgl_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

static
r_obj* int_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

static
r_obj* dbl_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

static
r_obj* cpl_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

static
r_obj* raw_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
);

static inline
bool should_slice_value(enum assignment_slice_value slice_value);
