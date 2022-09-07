r_obj* syms_vec_proxy;
r_obj* syms_vec_proxy_equal;
r_obj* syms_vec_proxy_equal_array;
r_obj* syms_vec_proxy_compare;
r_obj* syms_vec_proxy_compare_array;
r_obj* syms_vec_proxy_order;
r_obj* syms_vec_proxy_order_array;

r_obj* fns_vec_proxy_equal_array;
r_obj* fns_vec_proxy_compare_array;
r_obj* fns_vec_proxy_order_array;

static
r_obj* vec_proxy_2(r_obj* x, bool recurse);

static inline
r_obj* vec_proxy_equal_impl(r_obj* x);
static inline
r_obj* vec_proxy_compare_impl(r_obj* x);
static inline
r_obj* vec_proxy_order_impl(r_obj* x);

static inline
r_obj* vec_proxy_equal_method(r_obj* x);

static inline
r_obj* vec_proxy_equal_invoke(r_obj* x, r_obj* method);

static inline
r_obj* vec_proxy_compare_method(r_obj* x);

static inline
r_obj* vec_proxy_compare_invoke(r_obj* x, r_obj* method);

static inline
r_obj* vec_proxy_order_method(r_obj* x);

static inline
r_obj* vec_proxy_order_invoke(r_obj* x, r_obj* method);

static inline
r_obj* df_proxy(r_obj* x, enum vctrs_proxy_kind kind);

static inline
r_obj* df_proxy_recurse(r_obj* x);
