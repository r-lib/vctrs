enum vctrs_class_type class_type(r_obj* x);

static
enum vctrs_class_type class_type_impl(r_obj* cls);

static
const char* class_type_as_str(enum vctrs_class_type type);
