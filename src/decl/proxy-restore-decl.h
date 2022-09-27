static r_obj* syms_vec_restore_dispatch;
static r_obj* fns_vec_restore_dispatch;

static
r_obj* vec_restore_4(r_obj* x,
                     r_obj* to,
                     enum vctrs_owned owned,
                     enum vctrs_recurse recurse);

static
r_obj* vec_restore_dispatch(r_obj* x, r_obj* to);
