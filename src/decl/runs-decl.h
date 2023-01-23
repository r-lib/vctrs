static
r_obj* vec_detect_run_bounds(r_obj* x, bool start, struct r_lazy error_call);
static
r_obj* vec_locate_run_bounds(r_obj* x, bool start, struct r_lazy error_call);

static
r_obj* vec_detect_run_bounds0(r_obj* x, bool start, struct r_lazy error_call);

static inline
void lgl_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void int_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void dbl_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void cpl_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void chr_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void raw_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void list_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);

static inline
void df_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void lgl_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void int_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void dbl_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void cpl_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void chr_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void raw_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
static inline
void list_col_detect_run_bounds0(r_obj* x, r_ssize size, bool start, bool* v_out);
