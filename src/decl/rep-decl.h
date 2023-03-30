static inline
void stop_rep_times_size(struct r_lazy call,
                         struct vctrs_arg* p_times_arg);

static inline
void check_rep_times(int times,
                     struct r_lazy call,
                     struct vctrs_arg* p_times_arg);

static inline
void check_rep_each_times(int times,
                          r_ssize i,
                          struct r_lazy call,
                          struct vctrs_arg* p_times_arg);

static inline
bool multiply_would_overflow(r_ssize x, r_ssize y);

static inline
bool plus_would_overflow(r_ssize x, r_ssize y);

static inline
void stop_rep_size_oob(struct r_lazy call);

static
r_obj* vec_rep_each_uniform(r_obj* x,
                            int times,
                            struct r_lazy error_call,
                            struct vctrs_arg* p_times_arg);

static
r_obj* vec_rep_each_impl(r_obj* x,
                         r_obj* times,
                         r_ssize times_size,
                         struct r_lazy error_call,
                         struct vctrs_arg* p_times_arg);

static inline
void stop_rep_times_negative(struct r_lazy call, struct vctrs_arg* p_times_arg);

static inline
void stop_rep_times_missing(struct r_lazy call, struct vctrs_arg* p_times_arg);

static inline
void stop_rep_times_oob(int times, struct r_lazy call, struct vctrs_arg* p_times_arg);

static inline
void stop_rep_each_times_negative(r_ssize i, struct r_lazy call, struct vctrs_arg* p_times_arg);

static inline
void stop_rep_each_times_missing(r_ssize i, struct r_lazy call, struct vctrs_arg* p_times_arg);

static inline
void stop_rep_each_times_oob(int times, r_ssize i, struct r_lazy call, struct vctrs_arg* p_times_arg);

static
r_obj* vec_unrep(r_obj* x, struct r_lazy error_call);
