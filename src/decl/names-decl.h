static r_obj* syms_as_universal_names;
static r_obj* syms_check_unique_names;
static r_obj* fns_as_universal_names;
static r_obj* fns_check_unique_names;
static r_obj* syms_glue_as_name_spec;
static r_obj* fns_glue_as_name_spec;
static r_obj* syms_internal_spec;
static r_obj* syms_set_rownames_dispatch;
static r_obj* fns_set_rownames_dispatch;
static r_obj* syms_set_names_dispatch;
static r_obj* fns_set_names_dispatch;

static
void describe_repair(r_obj* old_names, r_obj* new_names);

static
r_obj* check_unique_names(r_obj* names,
                          const struct name_repair_opts* opts);

static
void vec_validate_minimal_names(r_obj* names, r_ssize n);

r_obj* ffi_as_minimal_names(r_obj* names);

static
bool any_has_suffix(r_obj* names);

static
r_obj* as_unique_names_impl(r_obj* names, bool quiet);

static
void stop_large_name();

static
bool is_dotdotint(const char* name);

static
ptrdiff_t suffix_pos(const char* name);

static
bool needs_suffix(r_obj* str);

static
r_obj* names_iota(r_ssize n);

static
r_obj* vec_unique_names_impl(r_obj* names, r_ssize n, bool quiet);

static
r_obj* glue_as_name_spec(r_obj* spec);
