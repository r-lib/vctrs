static
r_obj* syms_vec_is_vector_dispatch;

static
r_obj* fns_vec_is_vector_dispatch;

static
enum vctrs_type vec_base_typeof(r_obj* x, bool proxied);

// From proxy.c
r_obj* vec_proxy_method(r_obj* x);
r_obj* vec_proxy_invoke(r_obj* x, r_obj* method);
