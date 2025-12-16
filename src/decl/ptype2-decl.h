static
r_obj* vec_ptype2_switch_native(
  r_obj* x,
  r_obj* y,
  enum vctrs_type x_type,
  enum vctrs_type y_type,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback,
  int* left
);
