static inline
r_obj* atomic_unstructure(r_obj* x);

static inline
r_obj* array_unstructure(r_obj* x);

static inline
r_obj* df_unstructure(r_obj* x);

static inline
bool has_unstructured_atomic_attributes(r_obj* x);

static inline
bool has_unstructured_array_attributes(r_obj* x);

static inline
bool has_unstructured_data_frame_attributes(r_obj* x);

static inline
bool has_unstructured_data_frame_class(r_obj* x);

static inline
r_no_return
void stop_unsupported_storage_type(r_obj* x);
