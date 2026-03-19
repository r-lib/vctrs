static inline bool obj_vec_equal(r_obj* x, r_obj* y, enum r_type type);
static inline bool obj_expr_equal(r_obj* x, r_obj* y);
static inline bool obj_node_equal(r_obj* x, r_obj* y);
static inline bool obj_fn_equal(r_obj* x, r_obj* y);

static inline bool obj_attrib_equal(r_obj* x, r_obj* y);
static r_obj* obj_attrib_equal_cb(r_obj* tag, r_obj* value, void* data);
static r_obj* obj_attrib_count_cb(r_obj* _tag, r_obj* _value, void* data);
