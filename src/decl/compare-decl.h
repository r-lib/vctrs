static
r_obj* df_compare(r_obj* x, r_obj* y, bool na_equal, r_ssize size);

static
void df_compare_impl(int* v_out,
                     struct df_short_circuit_info* p_info,
                     r_obj* x,
                     r_obj* y,
                     bool na_equal);

static
void vec_compare_col(int* v_out,
                     struct df_short_circuit_info* p_info,
                     r_obj* x,
                     r_obj* y,
                     bool na_equal);
