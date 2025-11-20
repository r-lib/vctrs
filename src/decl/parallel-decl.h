static
r_obj* vec_parallel(
  r_obj* xs,
  enum vec_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call,
  enum vec_parallel_variant parallel
);

static inline
void vec_parallel_init(const int* v_x, enum vec_parallel_missing missing, r_ssize size, int* v_out);
static inline
void vec_parallel_init_missing_as_na(const int* v_x, r_ssize size, int* v_out);
static inline
void vec_parallel_init_missing_as_false(const int* v_x, r_ssize size, int* v_out);
static inline
void vec_parallel_init_missing_as_true(const int* v_x, r_ssize size, int* v_out);

static inline
void vec_pany_fill(const int* v_x, enum vec_parallel_missing missing, r_ssize size, int* v_out);
static inline
void vec_pall_fill(const int* v_x, enum vec_parallel_missing missing, r_ssize size, int* v_out);
static inline
void vec_pany_fill_missing_as_na(const int* v_x, r_ssize size, int* v_out);
static inline
void vec_pall_fill_missing_as_na(const int* v_x, r_ssize size, int* v_out);
static inline
void vec_pany_fill_missing_as_false(const int* v_x, r_ssize size, int* v_out);
static inline
void vec_pall_fill_missing_as_false(const int* v_x, r_ssize size, int* v_out);
static inline
void vec_pany_fill_missing_as_true(const int* v_x, r_ssize size, int* v_out);
static inline
void vec_pall_fill_missing_as_true(const int* v_x, r_ssize size, int* v_out);

static
enum vec_parallel_missing parse_vec_parallel_missing(r_obj* missing, struct r_lazy error_call);

static
r_ssize compute_size(r_ssize size, r_obj* xs);
