static inline
void vctrs_group_id_loop(struct dictionary* d, R_len_t n, int* p_out);

static inline
R_len_t vctrs_group_rle_loop(struct dictionary* d, R_len_t n, int* p_g, int* p_l);

static inline
SEXP new_group_rle(SEXP g, SEXP l, R_len_t n);

static inline
void vec_group_loc_loop(struct dictionary* d, R_len_t n, int* p_groups);
