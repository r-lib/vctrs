// Initialised at load time
struct vctrs_arg args_incomplete_;
static struct vctrs_arg* const args_incomplete = &args_incomplete_;

struct vctrs_arg args_no_match_;
static struct vctrs_arg* const args_no_match = &args_no_match_;

struct vctrs_arg args_remaining_;
static struct vctrs_arg* const args_remaining = &args_remaining_;


static
r_obj* vec_locate_matches(r_obj* needles,
                          r_obj* haystack,
                          r_obj* condition,
                          r_obj* filter,
                          const struct vctrs_incomplete* incomplete,
                          const struct vctrs_no_match* no_match,
                          const struct vctrs_remaining* remaining,
                          enum vctrs_multiple multiple,
                          bool nan_distinct,
                          r_obj* chr_proxy_collate,
                          struct vctrs_arg* needles_arg,
                          struct vctrs_arg* haystack_arg,
                          struct r_lazy call);

static
r_obj* df_locate_matches(r_obj* needles,
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
                         struct vctrs_arg* haystack_arg,
                         struct r_lazy call);

static
void df_locate_matches_recurse(r_ssize col,
                               r_ssize loc_lower_bound_o_needles,
                               r_ssize loc_upper_bound_o_needles,
                               r_ssize loc_lower_bound_o_haystack,
                               r_ssize loc_upper_bound_o_haystack,
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
                               struct r_dyn_array* p_loc_first_match_o_haystack,
                               struct r_dyn_array* p_size_match,
                               struct r_dyn_array* p_loc_needles,
                               int* v_loc_filter_match_o_haystack);

static
void df_locate_matches_with_containers(int n_containers,
                                       const int* v_container_ids,
                                       r_ssize col,
                                       r_ssize loc_lower_bound_o_needles,
                                       r_ssize loc_upper_bound_o_needles,
                                       r_ssize loc_lower_bound_o_haystack,
                                       r_ssize loc_upper_bound_o_haystack,
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
                                       struct r_dyn_array* p_loc_first_match_o_haystack,
                                       struct r_dyn_array* p_size_match,
                                       struct r_dyn_array* p_loc_needles,
                                       int* v_loc_filter_match_o_haystack);

static inline
r_ssize int_locate_upper_incomplete(const int* v_haystack_complete,
                                    const int* v_o_haystack,
                                    r_ssize loc_lower_bound_o_haystack,
                                    r_ssize loc_upper_bound_o_haystack);
static inline
r_ssize int_locate_lower_duplicate(int val_needle,
                                   const int* v_haystack,
                                   const int* v_o_haystack,
                                   r_ssize loc_lower_bound_o_haystack,
                                   r_ssize loc_upper_bound_o_haystack);
static inline
r_ssize int_locate_upper_duplicate(int val_needle,
                                   const int* v_haystack,
                                   const int* v_o_haystack,
                                   r_ssize loc_lower_bound_o_haystack,
                                   r_ssize loc_upper_bound_o_haystack);

static inline
struct vctrs_match_bounds int_locate_match(int val_needle,
                                           const int* v_haystack,
                                           const int* v_o_haystack,
                                           r_ssize loc_lower_bound_o_haystack,
                                           r_ssize loc_upper_bound_o_haystack);

static
r_obj* df_joint_xtfrm_by_col(r_obj* x,
                             r_obj* y,
                             r_ssize x_size,
                             r_ssize y_size,
                             r_ssize n_cols,
                             bool nan_distinct,
                             r_obj* chr_proxy_collate);

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
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_loc_first_match_o_haystack,
                              struct r_dyn_array* p_size_match,
                              struct r_dyn_array* p_loc_needles,
                              bool skip_size_match,
                              bool skip_loc_needles,
                              const struct vctrs_incomplete* incomplete,
                              const struct vctrs_no_match* no_match,
                              const struct vctrs_remaining* remaining,
                              enum vctrs_multiple multiple,
                              r_ssize size_needles,
                              r_ssize size_haystack,
                              bool any_non_equi,
                              bool has_loc_filter_match_o_haystack,
                              const enum vctrs_filter* v_filters,
                              const int* v_loc_filter_match_o_haystack,
                              const struct poly_df_data* p_haystack,
                              struct vctrs_arg* needles_arg,
                              struct vctrs_arg* haystack_arg,
                              struct r_lazy call);

static
r_obj* compute_nesting_container_info(r_obj* haystack,
                                      r_ssize size_haystack,
                                      const enum vctrs_ops* v_ops);

static
r_obj* compute_nesting_container_ids(r_obj* x,
                                     const int* v_order,
                                     const int* v_group_sizes,
                                     const int* v_outer_group_sizes,
                                     r_ssize size,
                                     r_ssize n_groups,
                                     bool has_outer_group_sizes);

static inline
bool p_nesting_container_df_compare_fully_ge_na_equal(const void* x,
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
                          struct vctrs_arg* haystack_arg,
                          struct r_lazy call);

static inline
void stop_matches_remaining(r_ssize i,
                            struct vctrs_arg* needles_arg,
                            struct vctrs_arg* haystack_arg,
                            struct r_lazy call);

static inline
void stop_matches_incomplete(r_ssize i,
                             struct vctrs_arg* needles_arg,
                             struct r_lazy call);

static inline
void stop_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg,
                           struct r_lazy call);

static inline
void warn_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg,
                           struct r_lazy call);
