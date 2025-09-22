static
r_obj* build_indices_for_to_as_list_of_vectors(
  r_obj* x,
  r_obj* from_flat,
  r_obj* from_flat_map,
  r_ssize x_size,
  r_ssize from_size
);

static
r_obj* build_xs_and_indices_for_to_as_vector(
  r_obj* x,
  r_obj* from_flat,
  r_obj* to,
  r_ssize x_size
);

static
r_obj* build_indices_for_single_to(r_obj* x, r_obj* from_flat);

static
r_obj* build_from_flat_map(r_obj* from, r_ssize from_size);

static
r_obj* build_repeated_to(r_obj* to, r_obj* from);

static
r_obj* ptype_finalize(
  r_obj* ptype,
  r_obj* to,
  r_obj* default_,
  bool to_as_list_of_vectors,
  struct vctrs_arg* p_to_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy call
);
