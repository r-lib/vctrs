static inline bool vec_equal_object(r_obj* x, r_obj* y, enum r_type type);
static inline bool expr_equal_object(r_obj* x, r_obj* y);
static inline bool node_equal_object(r_obj* x, r_obj* y);
static inline bool fn_equal_object(r_obj* x, r_obj* y);

static inline bool vec_equal_attrib(SEXP x, SEXP y);
