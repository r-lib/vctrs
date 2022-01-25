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
