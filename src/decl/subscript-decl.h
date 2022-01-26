static
r_obj* fns_cnd_body_subscript_dim;

static
r_obj* new_error_subscript_type(r_obj* subscript,
                                const struct subscript_opts* opts,
                                r_obj* body,
                                r_obj* parent);

static
enum subscript_type_action parse_subscript_arg_type(r_obj* x,
                                                    const char* kind);

static
r_obj* obj_cast_subscript(r_obj* subscript,
                          const struct subscript_opts* opts,
                          ERR* err);

static
r_obj* dbl_cast_subscript(r_obj* subscript,
                          const struct subscript_opts* opts,
                          ERR* err);

static
r_obj* dbl_cast_subscript_fallback(r_obj* subscript,
                                   const struct subscript_opts* opts,
                                   ERR* err);

static
r_obj* syms_new_dbl_cast_subscript_body;

static
r_obj* syms_lossy_err;

static
r_obj* syms_new_error_subscript_type;
