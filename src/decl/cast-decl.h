static
r_obj* vec_cast_switch_native(const struct cast_opts* opts,
                              enum vctrs_type x_type,
                              enum vctrs_type to_type,
                              bool* lossy);

static
r_obj* vec_cast_dispatch_s3(const struct cast_opts* opts);
