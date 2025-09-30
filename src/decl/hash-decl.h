static inline uint32_t sexp_hash(r_obj* x);

static inline uint32_t lgl_hash(r_obj* x);
static inline uint32_t int_hash(r_obj* x);
static inline uint32_t dbl_hash(r_obj* x);
static inline uint32_t cpl_hash(r_obj* x);
static inline uint32_t raw_hash(r_obj* x);
static inline uint32_t chr_hash(r_obj* x);
static inline uint32_t list_hash(r_obj* x);

static inline uint32_t expr_hash(r_obj* x);
static inline uint32_t node_hash(r_obj* x);
static inline uint32_t fn_hash(r_obj* x);

static inline void lgl_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void int_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void dbl_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void cpl_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void chr_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void raw_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void list_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out);

static inline void lgl_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void int_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void dbl_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void cpl_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void chr_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void raw_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out);
static inline void list_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out);

static inline void df_hash_fill(r_obj* x, r_ssize size, bool na_equal, uint32_t* v_out);
