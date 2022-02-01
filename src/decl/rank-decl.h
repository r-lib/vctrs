static inline enum ties parse_ties(r_obj* ties);
static inline enum incomplete parse_incomplete(r_obj* incomplete);

static inline bool r_lgl_all(r_obj* x);

static r_obj* vec_rank(r_obj* x,
                       enum ties ties_type,
                       enum incomplete incomplete_type,
                       r_obj* direction,
                       r_obj* na_value,
                       bool nan_distinct,
                       r_obj* chr_proxy_collate);

static void vec_rank_min(const int* v_order,
                         const int* v_group_sizes,
                         r_ssize n_groups,
                         int* v_rank);

static void vec_rank_max(const int* v_order,
                         const int* v_group_sizes,
                         r_ssize n_groups,
                         int* v_rank);

static void vec_rank_sequential(const int* v_order,
                                const int* v_group_sizes,
                                r_ssize n_groups,
                                int* v_rank);

static void vec_rank_dense(const int* v_order,
                           const int* v_group_sizes,
                           r_ssize n_groups,
                           int* v_rank);
