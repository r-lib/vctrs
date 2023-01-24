static
r_obj* vec_detect_run_bounds(r_obj* x, enum vctrs_run_bound which, struct r_lazy error_call);
static
r_obj* vec_locate_run_bounds(r_obj* x, enum vctrs_run_bound which, struct r_lazy error_call);

static
r_obj* vec_detect_run_bounds_bool(r_obj* x, enum vctrs_run_bound which, struct r_lazy error_call);

static inline
void lgl_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void int_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void dbl_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void cpl_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void chr_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void raw_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void list_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);

static inline
void df_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void lgl_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void int_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void dbl_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void cpl_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void chr_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void raw_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);
static inline
void list_col_detect_run_bounds_bool(r_obj* x, r_ssize size, enum vctrs_run_bound which, bool* v_out);

static inline
r_ssize compute_iter_loc(r_ssize size, enum vctrs_run_bound which);
static inline
r_ssize compute_iter_step(enum vctrs_run_bound which);

static inline
enum vctrs_run_bound as_run_bound(r_obj* which, struct r_lazy error_call);
