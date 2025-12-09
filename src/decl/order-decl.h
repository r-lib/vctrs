static inline bool parse_nan_distinct(SEXP nan_distinct);

static SEXP vec_order_info_impl(SEXP x,
                                SEXP direction,
                                SEXP na_value,
                                bool nan_distinct,
                                SEXP chr_proxy_collate,
                                bool group_sizes);

static SEXP vec_locate_sorted_groups(SEXP x,
                                     SEXP direction,
                                     SEXP na_value,
                                     bool nan_distinct,
                                     SEXP chr_proxy_collate);

static inline size_t vec_compute_n_bytes_lazy_raw(SEXP x, const enum vctrs_type type);

static inline size_t vec_compute_n_bytes_lazy_counts(SEXP x, const enum vctrs_type type);

static SEXP parse_na_value(SEXP na_value);

static SEXP parse_direction(SEXP direction);

static SEXP vec_order_expand_args(SEXP x, SEXP decreasing, SEXP na_largest);

static SEXP vec_order_compute_na_last(SEXP na_largest, SEXP decreasing);

static void vec_order_switch(SEXP x,
                             SEXP decreasing,
                             SEXP na_last,
                             bool nan_distinct,
                             r_ssize size,
                             const enum vctrs_type type,
                             struct order* p_order,
                             struct lazy_raw* p_lazy_x_chunk,
                             struct lazy_raw* p_lazy_x_aux,
                             struct lazy_raw* p_lazy_o_aux,
                             struct lazy_raw* p_lazy_bytes,
                             struct lazy_raw* p_lazy_counts,
                             struct group_infos* p_group_infos,
                             struct truelength_info* p_truelength_info);

static void df_order(SEXP x,
                     SEXP decreasing,
                     SEXP na_last,
                     bool nan_distinct,
                     r_ssize size,
                     struct order* p_order,
                     struct lazy_raw* p_lazy_x_chunk,
                     struct lazy_raw* p_lazy_x_aux,
                     struct lazy_raw* p_lazy_o_aux,
                     struct lazy_raw* p_lazy_bytes,
                     struct lazy_raw* p_lazy_counts,
                     struct group_infos* p_group_infos,
                     struct truelength_info* p_truelength_info);

static void vec_order_base_switch(SEXP x,
                                  bool decreasing,
                                  bool na_last,
                                  bool nan_distinct,
                                  r_ssize size,
                                  const enum vctrs_type type,
                                  struct order* p_order,
                                  struct lazy_raw* p_lazy_x_chunk,
                                  struct lazy_raw* p_lazy_x_aux,
                                  struct lazy_raw* p_lazy_o_aux,
                                  struct lazy_raw* p_lazy_bytes,
                                  struct lazy_raw* p_lazy_counts,
                                  struct group_infos* p_group_infos,
                                  struct truelength_info* p_truelength_info);

static void int_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void lgl_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void dbl_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      bool nan_distinct,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void cpl_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      bool nan_distinct,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void chr_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      struct truelength_info* p_truelength_info);

static void int_order_chunk_impl(bool decreasing,
                                 bool na_last,
                                 r_ssize size,
                                 void* p_x,
                                 int* p_o,
                                 struct lazy_raw* p_lazy_x_aux,
                                 struct lazy_raw* p_lazy_o_aux,
                                 struct lazy_raw* p_lazy_bytes,
                                 struct lazy_raw* p_lazy_counts,
                                 struct group_infos* p_group_infos);

static void int_order_impl(const int* p_x,
                           bool decreasing,
                           bool na_last,
                           r_ssize size,
                           bool copy,
                           struct order* p_order,
                           struct lazy_raw* p_lazy_x_chunk,
                           struct lazy_raw* p_lazy_x_aux,
                           struct lazy_raw* p_lazy_o_aux,
                           struct lazy_raw* p_lazy_bytes,
                           struct lazy_raw* p_lazy_counts,
                           struct group_infos* p_group_infos);

static void int_adjust(const bool decreasing,
                       const bool na_last,
                       const r_ssize size,
                       void* p_x);

static void int_compute_range(const int* p_x,
                              r_ssize size,
                              int* p_x_min,
                              uint32_t* p_range);

static void int_order_counting(const int* p_x,
                               r_ssize size,
                               int x_min,
                               uint32_t range,
                               bool initialized,
                               bool decreasing,
                               bool na_last,
                               int* p_o,
                               int* p_o_aux,
                               struct group_infos* p_group_infos);

static void int_order_insertion(const r_ssize size,
                                uint32_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos);

static void int_order_radix(const r_ssize size,
                            uint32_t* p_x,
                            int* p_o,
                            uint32_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            r_ssize* p_counts,
                            struct group_infos* p_group_infos);

static inline uint32_t int_map_to_uint32(int x);

static uint8_t int_compute_skips(const uint32_t* p_x, r_ssize size, bool* p_skips);

static void int_order_radix_recurse(const r_ssize size,
                                    const uint8_t pass,
                                    uint32_t* p_x,
                                    int* p_o,
                                    uint32_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    r_ssize* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos);

static inline uint8_t int_extract_uint32_byte(uint32_t x, uint8_t shift);

static void dbl_order_chunk_impl(bool decreasing,
                                 bool na_last,
                                 bool nan_distinct,
                                 r_ssize size,
                                 void* p_x,
                                 int* p_o,
                                 struct lazy_raw* p_lazy_x_aux,
                                 struct lazy_raw* p_lazy_o_aux,
                                 struct lazy_raw* p_lazy_bytes,
                                 struct lazy_raw* p_lazy_counts,
                                 struct group_infos* p_group_infos);

static void dbl_order_impl(const double* p_x,
                           bool decreasing,
                           bool na_last,
                           bool nan_distinct,
                           r_ssize size,
                           bool copy,
                           struct order* p_order,
                           struct lazy_raw* p_lazy_x_chunk,
                           struct lazy_raw* p_lazy_x_aux,
                           struct lazy_raw* p_lazy_o_aux,
                           struct lazy_raw* p_lazy_bytes,
                           struct lazy_raw* p_lazy_counts,
                           struct group_infos* p_group_infos);

static void dbl_adjust(const bool decreasing,
                       const bool na_last,
                       const bool nan_distinct,
                       const r_ssize size,
                       void* p_x);

static void dbl_order_insertion(const r_ssize size,
                                uint64_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos);

static void dbl_order_radix(const r_ssize size,
                            uint64_t* p_x,
                            int* p_o,
                            uint64_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            r_ssize* p_counts,
                            struct group_infos* p_group_infos);

static inline void dbl_adjust_nan_identical(const bool decreasing,
                                            const bool na_last,
                                            const r_ssize size,
                                            double* p_x_dbl,
                                            uint64_t* p_x_u64);

static inline void dbl_adjust_nan_distinct(const bool decreasing,
                                           const bool na_last,
                                           const r_ssize size,
                                           double* p_x_dbl,
                                           uint64_t* p_x_u64);

static inline uint64_t dbl_map_to_uint64(double x);

static inline uint64_t dbl_flip_uint64(uint64_t x);

static uint8_t dbl_compute_skips(const uint64_t* p_x, r_ssize size, bool* p_skips);

static void dbl_order_radix_recurse(const r_ssize size,
                                    const uint8_t pass,
                                    uint64_t* p_x,
                                    int* p_o,
                                    uint64_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    r_ssize* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos);

static inline uint8_t dbl_extract_uint64_byte(uint64_t x, uint8_t shift);

static void chr_mark_sorted_uniques(const SEXP* p_x,
                                    r_ssize size,
                                    struct lazy_raw* p_lazy_x_aux,
                                    struct lazy_raw* p_lazy_bytes,
                                    struct truelength_info* p_truelength_info);

static inline void chr_extract_ordering(const SEXP* p_x, r_ssize size, int* p_x_aux);

static void chr_order_radix(const r_ssize size,
                            const R_len_t max_size,
                            SEXP* p_x,
                            SEXP* p_x_aux,
                            int* p_sizes,
                            int* p_sizes_aux,
                            uint8_t* p_bytes);

static SEXP chr_order_exec(void* p_data);

static void chr_order_cleanup(void* p_data);

static void chr_order_internal(SEXP x,
                               bool decreasing,
                               bool na_last,
                               r_ssize size,
                               struct order* p_order,
                               struct lazy_raw* p_lazy_x_chunk,
                               struct lazy_raw* p_lazy_x_aux,
                               struct lazy_raw* p_lazy_o_aux,
                               struct lazy_raw* p_lazy_bytes,
                               struct lazy_raw* p_lazy_counts,
                               struct group_infos* p_group_infos,
                               struct truelength_info* p_truelength_info);

static void chr_mark_uniques(const SEXP* p_x,
                             r_ssize size,
                             struct truelength_info* p_truelength_info);

static bool chr_str_ge(SEXP x, SEXP y, int x_size, const R_len_t pass);

static void chr_order_radix_recurse(const r_ssize size,
                                    const R_len_t pass,
                                    const R_len_t max_size,
                                    SEXP* p_x,
                                    SEXP* p_x_aux,
                                    int* p_sizes,
                                    int* p_sizes_aux,
                                    uint8_t* p_bytes);

static SEXP df_order_exec(void* p_data);

static void df_order_cleanup(void* p_data);

static void df_order_internal(SEXP x,
                              SEXP decreasing,
                              SEXP na_last,
                              bool nan_distinct,
                              r_ssize size,
                              struct order* p_order,
                              struct lazy_raw* p_lazy_x_chunk,
                              struct lazy_raw* p_lazy_x_aux,
                              struct lazy_raw* p_lazy_o_aux,
                              struct lazy_raw* p_lazy_bytes,
                              struct lazy_raw* p_lazy_counts,
                              struct group_infos* p_group_infos,
                              struct truelength_info* p_truelength_info);

static void vec_order_chunk_switch(bool decreasing,
                                   bool na_last,
                                   bool nan_distinct,
                                   r_ssize size,
                                   const enum vctrs_type type,
                                   int* p_o,
                                   struct lazy_raw* p_lazy_x_chunk,
                                   struct lazy_raw* p_lazy_x_aux,
                                   struct lazy_raw* p_lazy_o_aux,
                                   struct lazy_raw* p_lazy_bytes,
                                   struct lazy_raw* p_lazy_counts,
                                   struct group_infos* p_group_infos,
                                   struct truelength_info* p_truelength_info);

static inline size_t df_compute_n_bytes_lazy_raw(SEXP x);

static size_t df_compute_n_bytes_lazy_counts(SEXP x);

static SEXP df_expand_args(SEXP x, SEXP args);

static SEXP expand_arg(SEXP arg, const int* p_expansions, r_ssize arg_size, r_ssize size);

static int vec_decreasing_expansion(SEXP x);

static int df_decreasing_expansion(SEXP x);

static int parse_na_value_one(SEXP x);

static int parse_direction_one(SEXP x);
