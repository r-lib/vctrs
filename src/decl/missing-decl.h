static inline
r_obj* proxy_equal_na(r_obj* proxy);

static inline
r_obj* lgl_equal_na(r_obj* x);
static inline
r_obj* int_equal_na(r_obj* x);
static inline
r_obj* dbl_equal_na(r_obj* x);
static inline
r_obj* cpl_equal_na(r_obj* x);
static inline
r_obj* raw_equal_na(r_obj* x);
static inline
r_obj* chr_equal_na(r_obj* x);
static inline
r_obj* list_equal_na(r_obj* x);

static inline
r_obj* df_equal_na(r_obj* x);

static inline
r_ssize col_equal_na(r_obj* x,
                     r_ssize* v_loc,
                     r_ssize loc_size);

static inline
r_ssize lgl_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size);
static inline
r_ssize int_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size);
static inline
r_ssize dbl_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size);
static inline
r_ssize cpl_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size);
static inline
r_ssize raw_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size);
static inline
r_ssize chr_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size);
static inline
r_ssize list_col_equal_na(r_obj* x,
                          r_ssize* v_loc,
                          r_ssize loc_size);

static inline
r_ssize proxy_first_missing(r_obj* proxy);

static inline
r_ssize lgl_first_missing(r_obj* x);
static inline
r_ssize int_first_missing(r_obj* x);
static inline
r_ssize dbl_first_missing(r_obj* x);
static inline
r_ssize cpl_first_missing(r_obj* x);
static inline
r_ssize raw_first_missing(r_obj* x);
static inline
r_ssize chr_first_missing(r_obj* x);
static inline
r_ssize list_first_missing(r_obj* x);

static inline
r_ssize df_first_missing(r_obj* x);

static inline
const unsigned char* r_uchar_cbegin(r_obj* x);
