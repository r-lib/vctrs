static
r_obj* chr_encode_utf8(r_obj* x);

static inline
r_ssize chr_find_encoding_start(r_obj* x, r_ssize size);

static
r_obj* list_encode_utf8(r_obj* x);

static
r_obj* obj_attrib_encode_utf8(r_obj* x, bool owned);
