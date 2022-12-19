static
r_ssize mark_intersections(enum vctrs_type type,
                           const void* p_x,
                           const void* p_y,
                           const int* v_x_o,
                           const int* v_y_o,
                           const int* v_x_group_starts,
                           const int* v_y_group_starts,
                           r_ssize n_x_groups,
                           r_ssize n_y_groups,
                           bool* v_marked);
