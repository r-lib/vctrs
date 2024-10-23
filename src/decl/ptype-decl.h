static
SEXP syms_vec_ptype;

static
SEXP syms_vec_ptype_finalise_dispatch;

static
SEXP fns_vec_ptype_finalise_dispatch;

static inline
r_obj* vec_ptype_slice(r_obj* x, r_obj* empty);

static
r_obj* df_ptype(r_obj* x, bool bare);

static
r_obj* col_ptype(r_obj* x);

static
r_obj* s3_ptype(r_obj* x,
                struct vctrs_arg* x_arg,
                struct r_lazy call);

static inline
r_obj* vec_ptype_method(r_obj* x);

static inline
r_obj* vec_ptype_invoke(r_obj* x, r_obj* method);

static
r_obj* vec_ptype_finalise_unspecified(r_obj* x);

static
r_obj* vec_ptype_finalise_dispatch(r_obj* x);

static
r_obj* vec_ptype_final_call;

static
struct r_lazy vec_ptype_final_lazy_call;
