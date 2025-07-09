static inline uint32_t dict_key_size(SEXP x);

static inline
void vctrs_unique_loc_loop(struct dictionary* d, struct growable* g, R_len_t n);

static inline
bool duplicated_any_loop(struct dictionary* d, R_len_t n);

static inline
void vctrs_n_distinct_loop(struct dictionary* d, R_len_t n);

static inline
void vctrs_id_loop(struct dictionary* d, R_len_t n, int* p_out);

static inline
void vec_match_loop(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
);

static inline
void vec_match_loop_propagate(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
);

static inline
void vec_in_loop(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
);

static inline
void vec_in_loop_propagate(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
);

static inline
void load_with_haystack(struct dictionary* d, R_len_t n_haystack);

static inline
void vctrs_count_loop(struct dictionary* d, R_len_t n, int* p_count);

static inline
void vctrs_duplicated_loop(struct dictionary* d, R_len_t n, uint32_t* p_hashes, int* p_out);
