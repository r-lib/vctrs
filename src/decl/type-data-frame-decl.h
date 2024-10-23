static r_obj* syms_df_lossy_cast;
static r_obj* fns_df_lossy_cast;

static
r_obj* new_compact_rownames(r_ssize n);

static
r_ssize df_size_from_n(r_obj* n);

static
r_obj* c_data_frame_class(r_obj* cls);

static
r_obj* data_frame(r_obj* x,
                  r_ssize size,
                  const struct name_repair_opts* p_name_repair_opts,
                  struct r_lazy error_call);

static
r_obj* df_list(r_obj* x,
               r_ssize size,
               bool unpack,
               const struct name_repair_opts* p_name_repair_opts,
               struct r_lazy error_call);

static
r_obj* df_list_drop_null(r_obj* x);

static
r_obj* df_list_unpack(r_obj* x);

static
void init_bare_data_frame(r_obj* x, r_ssize n);

static
r_obj* df_ptype2_match(const struct ptype2_opts* opts,
                     r_obj* x_names,
                     r_obj* y_names);

static
r_obj* df_ptype2_loop(const struct ptype2_opts* opts,
                    r_obj* y_names);

static
r_obj* df_cast_match(const struct cast_opts* opts,
                     r_obj* x_names,
                     r_obj* to_names);

static
r_obj* df_cast_loop(const struct cast_opts* opts, r_obj* names);

static
r_ssize df_flatten_loop(r_obj* x,
                        r_obj* out,
                        r_obj* out_names,
                        r_ssize counter);
