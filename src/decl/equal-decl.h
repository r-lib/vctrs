static inline bool obj_vec_equal(r_obj* x, r_obj* y, enum r_type type);
static inline bool obj_expr_equal(r_obj* x, r_obj* y);
static inline bool obj_node_equal(r_obj* x, r_obj* y);
static inline bool obj_fn_equal(r_obj* x, r_obj* y);

static inline bool vec_equal_attrib(SEXP x, SEXP y);
