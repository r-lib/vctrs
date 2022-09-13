static r_obj* syms_vec_restore_dispatch;
static r_obj* fns_vec_restore_dispatch;

static
r_obj* vec_restore_4(r_obj* x,
                     r_obj* to,
                     const enum vctrs_owned owned,
                     bool recurse);

static
r_obj* vec_restore_dispatch(r_obj* x, r_obj* to, bool recurse);
