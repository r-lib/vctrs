static
r_obj* vec_locate_runs(r_obj* x, bool start);

static inline
void vec_locate_run_starts(const int* v_id, r_ssize size, int* v_out);
static inline
void vec_locate_run_ends(const int* v_id, r_ssize size, int* v_out);


static
r_obj* vec_detect_runs(r_obj* x, bool start);

static inline
void vec_detect_run_starts(const int* v_id, r_ssize size, int* v_out);
static inline
void vec_detect_run_ends(const int* v_id, r_ssize size, int* v_out);


static inline
int lgl_identify_runs(r_obj* x, r_ssize size, int* v_out);
static inline
int int_identify_runs(r_obj* x, r_ssize size, int* v_out);
static inline
int dbl_identify_runs(r_obj* x, r_ssize size, int* v_out);
static inline
int cpl_identify_runs(r_obj* x, r_ssize size, int* v_out);
static inline
int chr_identify_runs(r_obj* x, r_ssize size, int* v_out);
static inline
int raw_identify_runs(r_obj* x, r_ssize size, int* v_out);
static inline
int list_identify_runs(r_obj* x, r_ssize size, int* v_out);
static inline
int df_identify_runs(r_obj* x, r_ssize size, int* v_out);


static inline
void col_identify_runs(r_obj* x, r_ssize size, bool* v_where);

static inline
void lgl_col_identify_runs(r_obj* x, r_ssize size, bool* v_where);
static inline
void int_col_identify_runs(r_obj* x, r_ssize size, bool* v_where);
static inline
void dbl_col_identify_runs(r_obj* x, r_ssize size, bool* v_where);
static inline
void cpl_col_identify_runs(r_obj* x, r_ssize size, bool* v_where);
static inline
void chr_col_identify_runs(r_obj* x, r_ssize size, bool* v_where);
static inline
void raw_col_identify_runs(r_obj* x, r_ssize size, bool* v_where);
static inline
void list_col_identify_runs(r_obj* x, r_ssize size, bool* v_where);
