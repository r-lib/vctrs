static
r_obj* vec_rbind(r_obj* xs,
                 r_obj* ptype,
                 r_obj* id,
                 struct name_repair_opts* name_repair,
                 r_obj* name_spec,
                 struct r_lazy error_call);

static
r_obj* as_df_row(r_obj* x,
                 struct name_repair_opts* name_repair,
                 struct r_lazy error_call);

static
r_obj* as_df_row_impl(r_obj* x,
                      struct name_repair_opts* name_repair,
                      struct r_lazy error_call);

static
struct name_repair_opts validate_bind_name_repair(r_obj* name_repair, bool allow_minimal);

static
r_obj* vec_cbind(r_obj* xs,
                 r_obj* ptype,
                 r_obj* size,
                 struct name_repair_opts* name_repair,
                 struct r_lazy error_call);

static
r_obj* cbind_names_to(bool has_names,
                      r_obj* names_to,
                      r_obj* ptype,
                      struct r_lazy error_call);

static
r_obj* as_df_col(r_obj* x,
                 r_obj* outer,
                 bool* allow_pack,
                 struct r_lazy error_call);

static
r_obj* cbind_container_type(r_obj* x, void* data);

static r_obj* syms_vec_cbind_frame_ptype;
static r_obj* fns_vec_cbind_frame_ptype;

static
r_obj* shaped_as_df_col(r_obj* x, r_obj* outer);

static
r_obj* vec_as_df_col(r_obj* x, r_obj* outer);

static
void df_c_fallback(r_obj* out,
                   r_obj* ptype,
                   r_obj* xs,
                   r_ssize n_rows,
                   r_obj* name_spec,
                   struct name_repair_opts* name_repair);
