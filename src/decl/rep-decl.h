static struct vctrs_arg args_times_;
static struct vctrs_arg* const p_args_times;

static inline
void stop_rep_times_size();

static inline
void check_rep_times(int times);

static inline
void check_rep_each_times(int times, r_ssize i);

static inline
bool multiply_would_overflow(r_ssize x, r_ssize y);

static inline
bool plus_would_overflow(r_ssize x, r_ssize y);

static inline
void stop_rep_size_oob();

static
r_obj* vec_rep_each_uniform(r_obj* x, int times);

static
r_obj* vec_rep_each_impl(r_obj* x, r_obj* times, const r_ssize times_size);

static inline
void stop_rep_times_negative();

static inline
void stop_rep_times_missing();

static inline
void stop_rep_times_oob(int times);

static inline
void stop_rep_each_times_negative(r_ssize i);

static inline
void stop_rep_each_times_missing(r_ssize i);

static inline
void stop_rep_each_times_oob(int times, r_ssize i);

static
r_obj* vec_unrep(r_obj* x);

static
r_obj* new_unrep_data_frame(r_obj* key, r_obj* times, r_ssize size);
