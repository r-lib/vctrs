static
r_obj* vec_shape2(r_obj* x, r_obj* y, struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);

static
r_obj* vec_shape2_impl(r_obj* x_dimensions,
                       r_obj* y_dimensions,
                       r_obj* x,
                       r_obj* y,
                       struct vctrs_arg* p_x_arg,
                       struct vctrs_arg* p_y_arg);

static
r_obj* vec_shape(r_obj* dimensions);

static inline
int vec_dimension2(int x_dimension, int y_dimension,
                   int axis,
                   r_obj* x, r_obj* y,
                   struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);
