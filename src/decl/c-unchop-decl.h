static
r_obj* list_unchop(r_obj* x,
                   r_obj* indices,
                   r_obj* ptype,
                   r_obj* name_spec,
                   const struct name_repair_opts* name_repair,
                   struct r_lazy error_call);

static
r_obj* list_unchop_fallback(r_obj* ptype,
                            r_obj* x,
                            r_obj* indices,
                            r_obj* name_spec,
                            const struct name_repair_opts* name_repair,
                            enum fallback_homogeneous homogenous,
                            struct r_lazy error_call);
