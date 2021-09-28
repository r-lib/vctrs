// Initialised at load time
struct vctrs_arg args_incomplete_;
static struct vctrs_arg* const args_incomplete = &args_incomplete_;

struct vctrs_arg args_no_match_;
static struct vctrs_arg* const args_no_match = &args_no_match_;

struct vctrs_arg args_remaining_;
static struct vctrs_arg* const args_remaining = &args_remaining_;


static
r_obj* vec_matches(r_obj* needles,
                   r_obj* haystack,
                   r_obj* condition,
                   r_obj* filter,
                   const struct vctrs_incomplete* incomplete,
                   const struct vctrs_no_match* no_match,
                   const struct vctrs_remaining* remaining,
                   enum vctrs_multiple multiple,
                   bool nan_distinct,
                   r_obj* chr_transform,
                   struct vctrs_arg* needles_arg,
                   struct vctrs_arg* haystack_arg);

static
r_obj* df_matches(r_obj* needles,
                  r_obj* haystack,
                  r_obj* needles_complete,
                  r_obj* haystack_complete,
                  r_ssize size_needles,
                  r_ssize size_haystack,
                  const struct vctrs_incomplete* incomplete,
                  const struct vctrs_no_match* no_match,
                  const struct vctrs_remaining* remaining,
                  enum vctrs_multiple multiple,
                  bool any_filters,
                  const enum vctrs_filter* v_filters,
                  const enum vctrs_ops* v_ops,
                  struct vctrs_arg* needles_arg,
                  struct vctrs_arg* haystack_arg);

static
void df_matches_recurse(r_ssize col,
                        r_ssize loc_lower_o_needles,
                        r_ssize loc_upper_o_needles,
                        r_ssize loc_lower_o_haystack,
                        r_ssize loc_upper_o_haystack,
                        const struct poly_df_data* p_needles,
                        const struct poly_df_data* p_haystack,
                        const struct poly_df_data* p_needles_complete,
                        const struct poly_df_data* p_haystack_complete,
                        const int* v_o_needles,
                        const int* v_o_haystack,
                        const struct vctrs_incomplete* incomplete,
                        enum vctrs_multiple multiple,
                        bool any_filters,
                        const enum vctrs_filter* v_filters,
                        const enum vctrs_ops* v_ops,
                        struct r_dyn_array* p_locs_start_o_haystack,
                        struct r_dyn_array* p_match_sizes,
                        struct r_dyn_array* p_locs_needles,
                        int* v_locs_filter_match_haystack,
                        r_ssize* p_n_extra);

static
void df_matches_with_nested_groups(r_ssize size_haystack,
                                   int n_nested_groups,
                                   const int* v_nested_groups,
                                   r_ssize col,
                                   r_ssize loc_lower_o_needles,
                                   r_ssize loc_upper_o_needles,
                                   const struct poly_df_data* p_needles,
                                   const struct poly_df_data* p_haystack,
                                   const struct poly_df_data* p_needles_complete,
                                   const struct poly_df_data* p_haystack_complete,
                                   const int* v_o_needles,
                                   const int* v_o_haystack,
                                   const struct vctrs_incomplete* incomplete,
                                   enum vctrs_multiple multiple,
                                   bool any_filters,
                                   const enum vctrs_filter* v_filters,
                                   const enum vctrs_ops* v_ops,
                                   struct r_dyn_array* p_locs_start_o_haystack,
                                   struct r_dyn_array* p_match_sizes,
                                   struct r_dyn_array* p_locs_needles,
                                   int* v_locs_filter_match_haystack,
                                   r_ssize* p_n_extra);

static inline
r_ssize int_locate_upper_incomplete(const int* v_haystack_complete,
                                    const int* v_o_haystack,
                                    r_ssize loc_lower_o_haystack,
                                    r_ssize loc_upper_o_haystack);
static inline
r_ssize int_lower_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_o_haystack,
                            r_ssize loc_upper_o_haystack);
static inline
r_ssize int_upper_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_o_haystack,
                            r_ssize loc_upper_o_haystack);

static
r_obj* df_joint_xtfrm_by_col(r_obj* x,
                             r_obj* y,
                             r_ssize x_size,
                             r_ssize y_size,
                             r_ssize n_cols,
                             bool nan_distinct,
                             r_obj* chr_transform);

static
r_obj* df_detect_complete_by_col(r_obj* x, r_ssize x_size, r_ssize n_cols);

static inline
void parse_condition(r_obj* condition, r_ssize n_cols, enum vctrs_ops* v_ops);

static inline
struct vctrs_no_match parse_no_match(r_obj* no_match);

static inline
struct vctrs_remaining parse_remaining(r_obj* remaining);

static inline
struct vctrs_incomplete parse_incomplete(r_obj* incomplete);

static inline
enum vctrs_multiple parse_multiple(r_obj* multiple);

static inline
void parse_filter(r_obj* filter, r_ssize n_cols, enum vctrs_filter* v_filters);

static
r_obj* expand_match_on_nothing(r_ssize size_needles,
                               r_ssize size_haystack,
                               enum vctrs_multiple multiple,
                               const struct vctrs_no_match* no_match,
                               const struct vctrs_remaining* remaining,
                               struct vctrs_arg* needles_arg,
                               struct vctrs_arg* haystack_arg);

static
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_locs_start_o_haystack,
                              struct r_dyn_array* p_match_sizes,
                              struct r_dyn_array* p_locs_needles,
                              bool skip_match_sizes,
                              bool skip_locs_needles,
                              const struct vctrs_incomplete* incomplete,
                              const struct vctrs_no_match* no_match,
                              const struct vctrs_remaining* remaining,
                              enum vctrs_multiple multiple,
                              r_ssize size_needles,
                              r_ssize size_haystack,
                              bool any_directional,
                              bool has_locs_filter_match_haystack,
                              const enum vctrs_filter* v_filters,
                              const int* v_locs_filter_match_haystack,
                              const struct poly_df_data* p_haystack,
                              struct vctrs_arg* needles_arg,
                              struct vctrs_arg* haystack_arg);

static
r_obj* compute_nested_containment_info(r_obj* haystack,
                                       enum vctrs_multiple multiple,
                                       const enum vctrs_ops* v_ops,
                                       struct vctrs_arg* haystack_arg);

static
r_obj* nested_containment_order(r_obj* x,
                                r_obj* order,
                                r_obj* group_sizes,
                                r_obj* outer_run_sizes,
                                enum vctrs_multiple multiple);

static inline
int p_df_nested_containment_compare_ge_na_equal(const void* x,
                                                r_ssize i,
                                                const void* y,
                                                r_ssize j);

static inline
int p_matches_df_compare_na_equal(const void* x,
                                  r_ssize i,
                                  const void* y,
                                  r_ssize j,
                                  const enum vctrs_filter* v_filters);

static inline
bool p_matches_df_equal_na_equal(const void* x,
                                 r_ssize i,
                                 const void* y,
                                 r_ssize j,
                                 const enum vctrs_filter* v_filters);

static inline
r_ssize midpoint(r_ssize lhs, r_ssize rhs);

static inline
void stop_matches_overflow(double size);

static inline
void stop_matches_nothing(r_ssize i,
                          struct vctrs_arg* needles_arg,
                          struct vctrs_arg* haystack_arg);

static inline
void stop_matches_remaining(r_ssize i,
                            struct vctrs_arg* needles_arg,
                            struct vctrs_arg* haystack_arg);

static inline
void stop_matches_incomplete(r_ssize i, struct vctrs_arg* needles_arg);

static inline
void stop_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg);

static inline
void warn_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg);
