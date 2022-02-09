// Initialized at load time
struct vctrs_arg args_start_;
static struct vctrs_arg* const args_start = &args_start_;

struct vctrs_arg args_end_;
static struct vctrs_arg* const args_end = &args_end_;

static r_obj* vec_locate_interval_merge_info(r_obj* start,
                                             r_obj* end,
                                             bool abutting,
                                             enum vctrs_interval_incomplete incomplete,
                                             bool groups);

static inline r_obj* interval_order(r_obj* complete, r_obj* start, r_obj* end);
static inline r_obj* interval_detect_complete(r_obj* start, r_obj* end);

static inline enum vctrs_interval_incomplete parse_incomplete(r_obj* incomplete);
