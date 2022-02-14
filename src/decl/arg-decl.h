static
int fill_arg_buffer(struct vctrs_arg* arg,
                    char* buf,
                    r_ssize cur_size,
                    r_ssize tot_size);

static
r_ssize counter_arg_fill(void* data, char* buf, r_ssize remaining);

static
r_ssize wrapper_arg_fill(void* data, char* buf, r_ssize remaining);

static
r_ssize lazy_arg_fill(void* data, char* buf, r_ssize remaining);

static
r_ssize index_arg_fill(void* data, char* buf, r_ssize remaining);

static
r_ssize subscript_arg_fill(void* p_data, char* buf, r_ssize remaining);

static
bool is_empty_arg(struct vctrs_arg* arg);
