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
void col_equal_na(r_obj* x,
                  int* v_out,
                  struct df_short_circuit_info* p_info);

static inline
void lgl_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info);
static inline
void int_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info);
static inline
void dbl_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info);
static inline
void cpl_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info);
static inline
void raw_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info);
static inline
void chr_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info);
static inline
void list_col_equal_na(r_obj* x,
                       int* v_out,
                       struct df_short_circuit_info* p_info);

static inline
const Rbyte* r_raw_cbegin2(r_obj* x);
