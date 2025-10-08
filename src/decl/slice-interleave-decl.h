static
r_ssize list_interleave_x_size_used(r_obj* const* v_x, r_ssize x_size);

static
r_obj* list_interleave_indices(
  r_obj* const* v_x,
  r_ssize x_size,
  r_ssize x_size_used,
  r_ssize elt_size
);
