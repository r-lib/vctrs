static r_obj* syms_vec_assign_fallback;
static r_obj* fns_vec_assign_fallback;

static
r_obj* vec_assign_fallback(r_obj* x, r_obj* index, r_obj* value);

static
r_obj* vec_proxy_assign_names(r_obj* proxy, r_obj* index, r_obj* value, const enum vctrs_ownership ownership);

static
r_obj* lgl_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership);

static
r_obj* int_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership);

static
r_obj* dbl_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership);

static
r_obj* cpl_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership);

static
r_obj* raw_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership);
