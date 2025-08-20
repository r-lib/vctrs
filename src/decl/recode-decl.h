static
r_obj* list_unchop_setup_with_to_as_list_of_vectors(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_ssize x_size,
  r_ssize from_size,
  r_ssize to_size,
  bool from_as_list_of_vectors
);

static
r_obj* list_unchop_setup_with_to_as_vector(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_ssize x_size,
  r_ssize to_size
);

static
r_obj* build_indices(r_obj* x, r_obj* from, r_ssize x_size, r_ssize from_size, bool from_as_list_of_vectors);

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
