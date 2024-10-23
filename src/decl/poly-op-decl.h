static
int p_df_equal_na_equal(const void* x, r_ssize i, const void* y, r_ssize j);

static
int p_df_compare_na_equal(const void* x, r_ssize i, const void* y, r_ssize j);

static
bool p_df_is_missing(const void* x, r_ssize i);

static
bool p_df_is_incomplete(const void* x, r_ssize i);

static void init_nil_poly_vec(struct poly_vec* p_poly_vec);
static void init_lgl_poly_vec(struct poly_vec* p_poly_vec);
static void init_int_poly_vec(struct poly_vec* p_poly_vec);
static void init_dbl_poly_vec(struct poly_vec* p_poly_vec);
static void init_cpl_poly_vec(struct poly_vec* p_poly_vec);
static void init_chr_poly_vec(struct poly_vec* p_poly_vec);
static void init_raw_poly_vec(struct poly_vec* p_poly_vec);
static void init_list_poly_vec(struct poly_vec* p_poly_vec);
static void init_df_poly_vec(struct poly_vec* p_poly_vec);
