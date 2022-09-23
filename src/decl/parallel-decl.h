static inline
r_obj* ffi_vec_p(r_obj* xs,
                 r_obj* ffi_na_rm,
                 r_obj* ffi_size,
                 r_obj* frame,
                 enum vctrs_parallel parallel);

static
r_obj* vec_p(r_obj* xs,
             bool na_rm,
             r_ssize size,
             enum vctrs_parallel parallel,
             struct r_lazy call);

static inline
void vec_pall_init(const int* restrict v_x, bool na_rm, r_ssize size, int* restrict v_out);
static inline
void vec_pany_init(const int* restrict v_x, bool na_rm, r_ssize size, int* restrict v_out);

static inline
void vec_pall_fill(const int* restrict v_x, bool na_rm, r_ssize size, int* restrict v_out);
static inline
void vec_pany_fill(const int* restrict v_x, bool na_rm, r_ssize size, int* restrict v_out);

static inline
void vec_pall_init_na_rm(const int* restrict v_x, r_ssize size, int* restrict v_out);
static inline
void vec_pall_fill_na_rm(const int* restrict v_x, r_ssize size, int* restrict v_out);

static inline
void vec_pany_init_na_rm(const int* restrict v_x, r_ssize size, int* restrict v_out);
static inline
void vec_pany_fill_na_rm(const int* restrict v_x, r_ssize size, int* restrict v_out);

static inline
void vec_pall_init_na_keep(const int* restrict v_x, r_ssize size, int* restrict v_out);
static inline
void vec_pall_fill_na_keep(const int* restrict v_x, r_ssize size, int* restrict v_out);

static inline
void vec_pany_init_na_keep(const int* restrict v_x, r_ssize size, int* restrict v_out);
static inline
void vec_pany_fill_na_keep(const int* restrict v_x, r_ssize size, int* restrict v_out);
