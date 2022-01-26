static
r_obj* int_invert_location(r_obj* subscript,
                           r_ssize n,
                           const struct location_opts* opts);
static
r_obj* int_filter_zero(r_obj* subscript, r_ssize n_zero);
static
void int_check_consecutive(r_obj* subscript,
                           r_ssize n,
                           r_ssize n_extend,
                           const struct location_opts* opts);

static
r_obj* lgl_as_location(r_obj* subscript,
                       r_ssize n,
                       const struct location_opts* opts);


static
void stop_subscript_missing(r_obj* i);
static
void stop_subscript_oob_location(r_obj* i,
                                 r_ssize size,
                                 const struct location_opts* opts);
static
void stop_subscript_oob_name(r_obj* i,
                             r_obj* names,
                             const struct location_opts* opts);
static
void stop_location_negative(r_obj* i,
                            const struct location_opts* opts);
static
void stop_location_zero(r_obj* i,
                        const struct location_opts* opts);
static
void stop_indicator_size(r_obj* i, r_obj* n,
                         const struct location_opts* opts);
static
void stop_location_negative_missing(r_obj* i,
                                    const struct location_opts* opts);
static
void stop_location_negative_positive(r_obj* i,
                                     const struct location_opts* opts);
static
void stop_location_oob_non_consecutive(r_obj* i,
                                       r_ssize size,
                                       const struct location_opts* opts);

static
enum subscript_missing parse_subscript_arg_missing(r_obj* x,
                                                   struct r_lazy call);
static
enum num_loc_negative parse_loc_negative(r_obj* x,
                                         struct r_lazy call);
static
enum num_loc_oob parse_loc_oob(r_obj* x,
                               struct r_lazy call);
static
enum num_loc_zero parse_loc_zero(r_obj* x,
                                 struct r_lazy call);

static
void stop_subscript_arg_missing(struct r_lazy call);
static
void stop_bad_negative(struct r_lazy call);
static
void stop_bad_oob(struct r_lazy call);
static
void stop_bad_zero(struct r_lazy call);
