static
SEXP syms_vec_ptype_finalise_dispatch;

static
SEXP fns_vec_ptype_finalise_dispatch;

static
r_obj* vec_ptype_finalise_unspecified(r_obj* x);

static
r_obj* vec_ptype_finalise_dispatch(r_obj* x);

static inline
bool lgl_is_unspecified(SEXP x);
