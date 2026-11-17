static
r_obj* ptype2_common(r_obj* current,
                     r_obj* next,
                     struct counters* counters,
                     void* data);

static inline
r_obj* ptype2_common_impl(
  r_obj* current,
  r_obj* next,
  struct counters* counters,
  struct ptype_common_reduce_opts* p_reduce_opts
);

static
void df_ptype_acc_start(struct df_ptype_acc* p_acc, r_obj* x, bool retyped);

static
bool df_ptype_acc_add(
  struct df_ptype_acc* p_acc,
  r_obj* next,
  struct counters* counters,
  struct ptype_common_reduce_opts* p_reduce_opts,
  int* p_left
);

static inline
void df_ptype_acc_mark_retype(struct df_ptype_acc* p_acc, r_ssize loc, r_obj* type);

static
r_obj* df_ptype_acc_finish(struct df_ptype_acc* p_acc);

static inline
bool is_plain_df(r_obj* x, bool* p_tibble);

static
enum df_ptype_acc_locate df_ptype_acc_locate(
  struct df_ptype_acc* p_acc,
  r_obj* name,
  int* p_loc
);

static inline
r_ssize df_ptype_acc_slot(struct df_ptype_acc* p_acc, r_obj* key);

static
void df_ptype_acc_rehash(struct df_ptype_acc* p_acc, r_ssize table_size);

static
void df_ptype_acc_grow(struct df_ptype_acc* p_acc);

static
void df_ptype_acc_reset(struct df_ptype_acc* p_acc, r_ssize size);

static
void df_ptype_acc_reserve_scratch(struct df_ptype_acc* p_acc, r_ssize size);

static
int df_ptype_acc_compare_loc(const void* p_x, const void* p_y);
