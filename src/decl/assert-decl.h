static r_no_return
void stop_non_list_type(r_obj* x,
                        struct vctrs_arg* arg,
                        struct r_lazy call);

static
void list_check_all_size(r_obj* xs,
                         r_ssize size,
                         struct vctrs_arg* p_arg,
                         struct r_lazy call);
