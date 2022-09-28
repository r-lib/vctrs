static inline
bool vec_implements_base_c(r_obj* x);

static inline
int vec_c_fallback_validate_args(r_obj* x, r_obj* name_spec);

static
void stop_vec_c_fallback(r_obj* xs, int err_type, struct r_lazy call);
