// Initialized at load time
struct vctrs_arg args_start_;
static struct vctrs_arg* const args_start = &args_start_;

struct vctrs_arg args_end_;
static struct vctrs_arg* const args_end = &args_end_;

struct vctrs_arg args_lower_;
static struct vctrs_arg* const args_lower = &args_lower_;

struct vctrs_arg args_upper_;
static struct vctrs_arg* const args_upper = &args_upper_;

static r_obj* vec_interval_group_info(r_obj* start,
                                      r_obj* end,
                                      bool abutting,
                                      enum vctrs_interval_missing missing,
                                      bool locations);

static
r_obj* vec_interval_complement(r_obj* start,
                               r_obj* end,
                               r_obj* lower,
                               r_obj* upper);

static
r_obj* vec_interval_locate_containers(r_obj* start, r_obj* end);

static inline
r_obj* interval_order(r_obj* start,
                      r_obj* end,
                      r_obj* direction,
                      r_obj* na_value,
                      r_ssize size);

static inline
enum vctrs_interval_missing parse_missing(r_obj* missing);
