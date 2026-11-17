#include "vctrs.h"
#include "hash.h"
#include "type-data-frame.h"

#define DF_PTYPE_ACC_SMALL_TABLE_SIZE 64
#define DF_PTYPE_ACC_SMALL_SCRATCH_SIZE 96

// Accumulates the common type of consecutive data frames column by column.
// `vec_ptype2()` of two data frames retypes every column of both, so folding
// it over many data frames with different columns costs the number of inputs
// times the number of columns. Here each data frame only touches its own
// columns, and the columns it lacks keep their type. That relies on retyping
// a type giving it back, as vctrs expects of `vec_ptype2(x, x)`.
struct df_ptype_acc {
  bool active;
  bool tibble;

  // Column names, the names encoded as UTF-8 to match them like `vec_match()`
  // does, and column types. `n_types` lags `n_names` while a data frame's new
  // columns are being typed.
  r_obj* names;
  r_obj* keys;
  r_obj* types;
  r_ssize n_names;
  r_ssize n_types;
  r_ssize capacity;

  // Open addressing hash table of locations in `keys`, `-1` when empty. Starts
  // out as `small_table`, to save an allocation.
  int* v_table;
  r_ssize table_size;
  int small_table[DF_PTYPE_ACC_SMALL_TABLE_SIZE];

  // For each column of the next data frame: pairs of locations in the common
  // type and in the data frame for columns already seen, then the locations
  // of new columns in the data frame
  int* v_scratch;
  r_ssize scratch_size;
  int small_scratch[DF_PTYPE_ACC_SMALL_SCRATCH_SIZE];

  r_obj* shelter;
  r_keep_loc shelter_pi;
};

enum df_ptype_acc_shelter {
  DF_PTYPE_ACC_SHELTER_names,
  DF_PTYPE_ACC_SHELTER_keys,
  DF_PTYPE_ACC_SHELTER_types,
  DF_PTYPE_ACC_SHELTER_table,
  DF_PTYPE_ACC_SHELTER_scratch,
  DF_PTYPE_ACC_SHELTER_N
};

enum df_ptype_acc_locate {
  DF_PTYPE_ACC_LOCATE_new,
  DF_PTYPE_ACC_LOCATE_existing,
  DF_PTYPE_ACC_LOCATE_unsupported
};

struct ptype_common_reduce_opts {
  struct r_lazy call;
  enum s3_fallback s3_fallback;
  struct df_ptype_acc* p_df_acc;
  r_ssize n_remaining;
};

#include "decl/ptype-common-decl.h"

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* ptype = r_node_car(args); args = r_node_cdr(args);
  r_obj* ffi_finalise = r_node_car(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy xs_arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  const enum ptype_finalise finalise = r_arg_as_bool(ffi_finalise, ".finalise") ?
    PTYPE_FINALISE_true :
    PTYPE_FINALISE_false;
  const enum s3_fallback s3_fallback = S3_FALLBACK_false;

  r_obj* out = vec_ptype_common(
    xs,
    ptype,
    finalise,
    s3_fallback,
    &xs_arg,
    call
  );

  return out;
}

// [[ register(external = TRUE) ]]
r_obj* ffi_ptype_common_params(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* xs = r_node_car(args); args = r_node_cdr(args);
  r_obj* ptype = r_node_car(args); args = r_node_cdr(args);
  r_obj* ffi_finalise = r_node_car(args); args = r_node_cdr(args);
  r_obj* opts = r_node_car(args);

  struct r_lazy call = { .x = syms.dot_call, .env = env };
  struct r_lazy xs_arg_lazy = { .x = syms.dot_arg, .env = env };
  struct vctrs_arg xs_arg = new_lazy_arg(&xs_arg_lazy);

  const enum ptype_finalise finalise = r_arg_as_bool(ffi_finalise, ".finalise") ?
    PTYPE_FINALISE_true :
    PTYPE_FINALISE_false;
  const enum s3_fallback s3_fallback = s3_fallback_from_opts(opts);

  r_obj* out = vec_ptype_common(
    xs,
    ptype,
    finalise,
    s3_fallback,
    &xs_arg,
    call
  );

  return out;
}

// Invariant of `vec_ptype_common()` is that the output is always a finalised `ptype`,
// even if the user provided their own, unless `PTYPE_FINALISE_false` is specified.
r_obj* vec_ptype_common(
  r_obj* dots,
  r_obj* ptype,
  enum ptype_finalise finalise,
  enum s3_fallback s3_fallback,
  struct vctrs_arg* p_arg,
  struct r_lazy call
) {
  int n_prot = 0;

  r_obj* out;

  if (ptype != r_null) {
    out = KEEP_N(vec_ptype(ptype, vec_args.dot_ptype, call), &n_prot);
  } else {
    if (r_is_true(r_peek_option("vctrs.no_guessing"))) {
      r_abort_lazy_call(r_lazy_null, "strict mode is activated; you must supply complete `.ptype`.");
    }

    // Not zero initialised, to skip clearing the small buffers
    struct df_ptype_acc df_acc;
    df_acc.active = false;
    df_acc.shelter = r_null;
    KEEP_HERE(r_null, &df_acc.shelter_pi);
    ++n_prot;

    struct ptype_common_reduce_opts reduce_opts = {
      .call = call,
      .s3_fallback = s3_fallback,
      .p_df_acc = &df_acc,
      .n_remaining = r_length(dots)
    };

    out = KEEP_N(
      reduce(
        r_null,
        vec_args.empty,
        p_arg,
        dots,
        &ptype2_common,
        &reduce_opts
      ),
      &n_prot
    );

    if (df_acc.active) {
      out = KEEP_N(df_ptype_acc_finish(&df_acc), &n_prot);
    }
  }

  if (should_finalise(finalise)) {
    out = KEEP_N(vec_ptype_finalise(out), &n_prot);
  }

  FREE(n_prot);
  return out;
}

static
r_obj* ptype2_common(r_obj* current,
                     r_obj* next,
                     struct counters* counters,
                     void* p_data) {
  struct ptype_common_reduce_opts* p_reduce_opts = (struct ptype_common_reduce_opts*) p_data;
  struct df_ptype_acc* p_acc = p_reduce_opts->p_df_acc;

  // Inputs left after `next`
  --p_reduce_opts->n_remaining;

  if (p_acc->active) {
    int left = -1;

    if (df_ptype_acc_add(p_acc, next, counters, p_reduce_opts, &left)) {
      if (!left) {
        counters_shift(counters);
      }

      // The common type so far lives in `p_acc`, not `current`
      return current;
    }

    current = KEEP(df_ptype_acc_finish(p_acc));
    r_obj* out = ptype2_common_impl(current, next, counters, p_reduce_opts);
    FREE(1);
    return out;
  }

  r_obj* out = ptype2_common_impl(current, next, counters, p_reduce_opts);

  // Accumulate from the common type of two data frames rather than from the
  // first one. Its columns have been retyped by `vec_ptype2()`, which can
  // differ from their `vec_ptype()`, as with the common class fallback.
  // Setting up costs about as much as one `vec_ptype2()`, so only when there
  // are at least two inputs left.
  bool tibble;
  if (p_reduce_opts->n_remaining >= 2 &&
      is_plain_df(next, &tibble) &&
      is_plain_df(current, &tibble)) {
    KEEP(out);
    df_ptype_acc_start(p_acc, out);
    FREE(1);
  }

  return out;
}

static inline
r_obj* ptype2_common_impl(
  r_obj* current,
  r_obj* next,
  struct counters* counters,
  struct ptype_common_reduce_opts* p_reduce_opts
) {
  int left = -1;

  current = vec_ptype2(
    current,
    next,
    counters->curr_arg,
    counters->next_arg,
    p_reduce_opts->call,
    p_reduce_opts->s3_fallback,
    &left
  );

  // Update current if RHS is the common type. Otherwise the previous
  // counter stays in effect.
  if (!left) {
    counters_shift(counters);
  }

  return current;
}

// -----------------------------------------------------------------------------

// Starts accumulating from the common type `x` of two data frames. Leaves
// `p_acc` inactive if `x` has names `df_ptype_acc_add()` can't match.
static
void df_ptype_acc_start(struct df_ptype_acc* p_acc, r_obj* x) {
  bool tibble;
  if (!is_plain_df(x, &tibble)) {
    return;
  }

  r_obj* names = r_names(x);
  if (names == r_null || r_length(names) != r_length(x)) {
    return;
  }

  const r_ssize size = r_length(names);
  df_ptype_acc_reset(p_acc, size);

  r_obj* const* v_names = r_chr_cbegin(names);
  r_obj* const* v_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    int loc;

    if (df_ptype_acc_locate(p_acc, v_names[i], &loc) != DF_PTYPE_ACC_LOCATE_new) {
      // Duplicate or unsupported name
      return;
    }

    r_list_poke(p_acc->types, i, v_x[i]);
  }

  p_acc->n_types = size;
  p_acc->tibble = tibble;
  p_acc->active = true;
}

// Adds `next` to the common type, giving the same result and errors as
// `vec_ptype2(current, next)`. Returns `false` without retyping any column if
// `next` needs `vec_ptype2()` itself.
static
bool df_ptype_acc_add(
  struct df_ptype_acc* p_acc,
  r_obj* next,
  struct counters* counters,
  struct ptype_common_reduce_opts* p_reduce_opts,
  int* p_left
) {
  if (next == r_null) {
    // `vec_ptype2(current, NULL)` retypes `current`, which leaves it as it is
    *p_left = 1;
    return true;
  }

  bool next_tibble;
  if (!is_plain_df(next, &next_tibble)) {
    return false;
  }

  r_obj* names = r_names(next);
  if (names == r_null || r_length(names) != r_length(next)) {
    return false;
  }

  const r_ssize size = r_length(names);
  const r_ssize acc_size = p_acc->n_types;

  df_ptype_acc_reserve_scratch(p_acc, size);
  int* v_existing = p_acc->v_scratch;
  int* v_new = p_acc->v_scratch + 2 * size;

  r_ssize n_existing = 0;
  r_ssize n_new = 0;
  bool sorted = true;

  r_obj* const* v_names = r_chr_cbegin(names);

  for (r_ssize i = 0; i < size; ++i) {
    int loc;

    switch (df_ptype_acc_locate(p_acc, v_names[i], &loc)) {
    case DF_PTYPE_ACC_LOCATE_new: {
      v_new[n_new] = i;
      ++n_new;
      break;
    }
    case DF_PTYPE_ACC_LOCATE_existing: {
      if (loc >= acc_size) {
        // Duplicate name in `next`
        return false;
      }
      if (n_existing > 0 && loc <= v_existing[2 * (n_existing - 1)]) {
        sorted = false;
      }
      v_existing[2 * n_existing] = loc;
      v_existing[2 * n_existing + 1] = i;
      ++n_existing;
      break;
    }
    case DF_PTYPE_ACC_LOCATE_unsupported: {
      return false;
    }
    }
  }

  if (!sorted) {
    // `df_ptype2()` combines the columns in the order of `current`, which
    // decides the error when several columns are incompatible
    qsort(v_existing, n_existing, 2 * sizeof(int), &df_ptype_acc_compare_loc);

    for (r_ssize i = 1; i < n_existing; ++i) {
      if (v_existing[2 * i] == v_existing[2 * (i - 1)]) {
        // Duplicate name in `next`
        return false;
      }
    }
  }

  r_ssize acc_loc = 0;
  r_ssize next_loc = 0;

  struct vctrs_arg* p_acc_col_arg = new_subscript_arg(
    counters->curr_arg,
    p_acc->names,
    acc_size,
    &acc_loc
  );
  KEEP(p_acc_col_arg->shelter);

  struct vctrs_arg* p_next_col_arg = new_subscript_arg(
    counters->next_arg,
    names,
    size,
    &next_loc
  );
  KEEP(p_next_col_arg->shelter);

  r_obj* const* v_next = r_list_cbegin(next);

  for (r_ssize i = 0; i < n_existing; ++i) {
    acc_loc = v_existing[2 * i];
    next_loc = v_existing[2 * i + 1];

    int _;
    r_obj* type = vec_ptype2(
      r_list_get(p_acc->types, acc_loc),
      v_next[next_loc],
      p_acc_col_arg,
      p_next_col_arg,
      p_reduce_opts->call,
      p_reduce_opts->s3_fallback,
      &_
    );
    r_list_poke(p_acc->types, acc_loc, type);
  }

  for (r_ssize i = 0; i < n_new; ++i) {
    next_loc = v_new[i];
    r_obj* col = v_next[next_loc];

    r_obj* type = vec_ptype_or_s3_fallback(
      col,
      p_next_col_arg,
      vec_typeof(col),
      p_reduce_opts->call,
      p_reduce_opts->s3_fallback
    );
    r_list_poke(p_acc->types, p_acc->n_types, type);
    ++p_acc->n_types;
  }

  // Same as `vec_ptype2()`: a tibble wins over a data frame
  if (p_acc->tibble) {
    *p_left = next_tibble ? -1 : 1;
  } else {
    *p_left = next_tibble ? 0 : -1;
    p_acc->tibble = next_tibble;
  }

  FREE(2);
  return true;
}

// Returns the common type as a data frame and stops accumulating. Builds it
// like `df_ptype2()` and `tib_ptype2()`.
static
r_obj* df_ptype_acc_finish(struct df_ptype_acc* p_acc) {
  p_acc->active = false;

  const r_ssize size = p_acc->n_types;

  r_obj* out = KEEP(r_alloc_list(size));
  r_obj* names = KEEP(r_alloc_character(size));
  r_attrib_poke(out, r_syms.names, names);

  for (r_ssize i = 0; i < size; ++i) {
    r_list_poke(out, i, r_list_get(p_acc->types, i));
    r_chr_poke(names, i, r_chr_get(p_acc->names, i));
  }

  init_data_frame(out, 0);

  if (p_acc->tibble) {
    r_attrib_poke_class(out, classes_tibble);
  }

  FREE(2);
  return out;
}

// Data frames and tibbles, for which `vec_ptype2()` calls `df_ptype2()`
// without dispatching, and which only uses their names and columns
static inline
bool is_plain_df(r_obj* x, bool* p_tibble) {
  if (r_typeof(x) != R_TYPE_list || r_dim(x) != r_null) {
    return false;
  }

  switch (class_type(x)) {
  case VCTRS_CLASS_bare_data_frame: *p_tibble = false; return true;
  case VCTRS_CLASS_bare_tibble: *p_tibble = true; return true;
  default: return false;
  }
}

// Finds `name` among the columns, adding it as a new column if it isn't there.
// Names are matched like `vec_match()` does, by their UTF-8 encoding.
static
enum df_ptype_acc_locate df_ptype_acc_locate(
  struct df_ptype_acc* p_acc,
  r_obj* name,
  int* p_loc
) {
  // `vec_match()` can't encode bytes, so leave them to `df_ptype2()`. Missing
  // names are unusual enough to also leave to it.
  if (name == r_globals.na_str || Rf_getCharCE(name) == CE_BYTES) {
    return DF_PTYPE_ACC_LOCATE_unsupported;
  }

  r_obj* key = name;
  if (!str_is_ascii_or_utf8(name)) {
    const void* vmax = vmaxget();
    key = str_as_utf8(name);
    vmaxset(vmax);
  }
  KEEP(key);

  const r_ssize slot = df_ptype_acc_slot(p_acc, key);
  const int loc = p_acc->v_table[slot];

  if (loc != -1) {
    *p_loc = loc;
    FREE(1);
    return DF_PTYPE_ACC_LOCATE_existing;
  }

  if (p_acc->n_names == p_acc->capacity) {
    df_ptype_acc_grow(p_acc);
  }

  *p_loc = p_acc->n_names;
  p_acc->v_table[slot] = *p_loc;
  r_chr_poke(p_acc->keys, p_acc->n_names, key);
  r_chr_poke(p_acc->names, p_acc->n_names, name);
  ++p_acc->n_names;

  if (2 * p_acc->n_names > p_acc->table_size) {
    df_ptype_acc_rehash(p_acc, 2 * p_acc->table_size);
  }

  FREE(1);
  return DF_PTYPE_ACC_LOCATE_new;
}

// The slot holding `key`, or the empty slot where it goes
static inline
r_ssize df_ptype_acc_slot(struct df_ptype_acc* p_acc, r_obj* key) {
  const r_ssize mask = p_acc->table_size - 1;
  r_obj* const* v_keys = r_chr_cbegin(p_acc->keys);
  r_ssize slot = chr_hash_scalar(key) & mask;

  while (true) {
    const int loc = p_acc->v_table[slot];

    if (loc == -1 || v_keys[loc] == key) {
      return slot;
    }

    slot = (slot + 1) & mask;
  }
}

static
void df_ptype_acc_rehash(struct df_ptype_acc* p_acc, r_ssize table_size) {
  if (table_size <= DF_PTYPE_ACC_SMALL_TABLE_SIZE) {
    p_acc->v_table = p_acc->small_table;
  } else {
    r_obj* table = r_alloc_integer(table_size);
    r_list_poke(p_acc->shelter, DF_PTYPE_ACC_SHELTER_table, table);
    p_acc->v_table = r_int_begin(table);
  }

  p_acc->table_size = table_size;
  r_p_int_fill(p_acc->v_table, -1, table_size);

  r_obj* const* v_keys = r_chr_cbegin(p_acc->keys);

  for (r_ssize i = 0; i < p_acc->n_names; ++i) {
    p_acc->v_table[df_ptype_acc_slot(p_acc, v_keys[i])] = i;
  }
}

static
void df_ptype_acc_grow(struct df_ptype_acc* p_acc) {
  const r_ssize capacity = r_ssize_mult(p_acc->capacity, 2);

  p_acc->names = r_chr_resize(p_acc->names, capacity);
  r_list_poke(p_acc->shelter, DF_PTYPE_ACC_SHELTER_names, p_acc->names);

  p_acc->keys = r_chr_resize(p_acc->keys, capacity);
  r_list_poke(p_acc->shelter, DF_PTYPE_ACC_SHELTER_keys, p_acc->keys);

  p_acc->types = r_list_resize(p_acc->types, capacity);
  r_list_poke(p_acc->shelter, DF_PTYPE_ACC_SHELTER_types, p_acc->types);

  p_acc->capacity = capacity;
}

// Clears `p_acc`, making room for `size` columns
static
void df_ptype_acc_reset(struct df_ptype_acc* p_acc, r_ssize size) {
  if (p_acc->shelter == r_null) {
    r_obj* shelter = r_alloc_list(DF_PTYPE_ACC_SHELTER_N);
    KEEP_AT(shelter, p_acc->shelter_pi);
    p_acc->shelter = shelter;

    const r_ssize capacity = r_ssize_max(2 * size, 16);

    p_acc->names = r_alloc_character(capacity);
    r_list_poke(shelter, DF_PTYPE_ACC_SHELTER_names, p_acc->names);

    p_acc->keys = r_alloc_character(capacity);
    r_list_poke(shelter, DF_PTYPE_ACC_SHELTER_keys, p_acc->keys);

    p_acc->types = r_alloc_list(capacity);
    r_list_poke(shelter, DF_PTYPE_ACC_SHELTER_types, p_acc->types);

    p_acc->capacity = capacity;
    p_acc->table_size = 0;
    p_acc->v_scratch = p_acc->small_scratch;
    p_acc->scratch_size = DF_PTYPE_ACC_SMALL_SCRATCH_SIZE;
  }

  while (p_acc->capacity < size) {
    df_ptype_acc_grow(p_acc);
  }

  p_acc->n_names = 0;
  p_acc->n_types = 0;

  r_ssize table_size = 32;
  while (table_size < 2 * size) {
    table_size *= 2;
  }
  df_ptype_acc_rehash(p_acc, r_ssize_max(table_size, p_acc->table_size));
}

static
void df_ptype_acc_reserve_scratch(struct df_ptype_acc* p_acc, r_ssize size) {
  if (3 * size <= p_acc->scratch_size) {
    return;
  }

  const r_ssize scratch_size = r_ssize_mult(size, 6);

  r_obj* scratch = r_alloc_integer(scratch_size);
  r_list_poke(p_acc->shelter, DF_PTYPE_ACC_SHELTER_scratch, scratch);

  p_acc->v_scratch = r_int_begin(scratch);
  p_acc->scratch_size = scratch_size;
}

static
int df_ptype_acc_compare_loc(const void* p_x, const void* p_y) {
  const int x = *((const int*) p_x);
  const int y = *((const int*) p_y);
  return (x > y) - (x < y);
}
