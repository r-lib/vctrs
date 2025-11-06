static
r_obj* list_parallel(
  r_obj* xs,
  enum list_parallel_missing missing,
  r_ssize size,
  struct vctrs_arg* p_xs_arg,
  struct r_lazy error_call,
  enum list_parallel_variant parallel
);

static inline
void list_parallel_init(const int* v_x, enum list_parallel_missing missing, r_ssize size, int* v_out);
static inline
void list_parallel_init_missing_as_na(const int* v_x, r_ssize size, int* v_out);
static inline
void list_parallel_init_missing_as_false(const int* v_x, r_ssize size, int* v_out);
static inline
void list_parallel_init_missing_as_true(const int* v_x, r_ssize size, int* v_out);

static inline
void list_pany_fill(const int* v_x, enum list_parallel_missing missing, r_ssize size, int* v_out);
static inline
void list_pall_fill(const int* v_x, enum list_parallel_missing missing, r_ssize size, int* v_out);
static inline
void list_pany_fill_missing_as_na(const int* v_x, r_ssize size, int* v_out);
static inline
void list_pall_fill_missing_as_na(const int* v_x, r_ssize size, int* v_out);
static inline
void list_pany_fill_missing_as_false(const int* v_x, r_ssize size, int* v_out);
static inline
void list_pall_fill_missing_as_false(const int* v_x, r_ssize size, int* v_out);
static inline
void list_pany_fill_missing_as_true(const int* v_x, r_ssize size, int* v_out);
static inline
void list_pall_fill_missing_as_true(const int* v_x, r_ssize size, int* v_out);

static
enum list_parallel_missing parse_list_parallel_missing(r_obj* missing, struct r_lazy error_call);

static
r_ssize compute_size(r_ssize size, r_obj* xs);
