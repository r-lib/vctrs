#include "vctrs.h"

struct r_ssize_pair {
  r_ssize x;
  r_ssize y;
};

#include "decl/set-decl.h"

// -----------------------------------------------------------------------------

r_obj* ffi_vec_set_intersect(r_obj* x,
                             r_obj* y,
                             r_obj* ptype,
                             r_obj* frame) {
  struct r_lazy call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_lazy = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_lazy);

  return vec_set_intersect(x, y, ptype, &x_arg, &y_arg, call);
}

r_obj* vec_set_intersect(r_obj* x,
                         r_obj* y,
                         r_obj* ptype,
                         struct vctrs_arg* x_arg,
                         struct vctrs_arg* y_arg,
                         struct r_lazy call) {
  int n_prot = 0;

  if (ptype == r_null) {
    int _;

    ptype = vec_ptype2(
      x,
      y,
      x_arg,
      y_arg,
      call,
      S3_FALLBACK_false,
      &_
    );
    KEEP_N(ptype, &n_prot);

    ptype = vec_ptype_finalise(ptype);
    KEEP_N(ptype, &n_prot);
  }

  x = vec_cast_params(
    x,
    ptype,
    x_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(x, &n_prot);

  y = vec_cast_params(
    y,
    ptype,
    y_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(y, &n_prot);

  r_obj* x_proxy = KEEP_N(vec_proxy_equal(x), &n_prot);
  x_proxy = KEEP_N(vec_normalize_encoding(x_proxy), &n_prot);

  r_obj* y_proxy = KEEP_N(vec_proxy_equal(y), &n_prot);
  y_proxy = KEEP_N(vec_normalize_encoding(y_proxy), &n_prot);

  const r_ssize x_size = vec_size(x_proxy);
  const r_ssize y_size = vec_size(y_proxy);

  struct dictionary* x_dict = new_dictionary(x_proxy);
  PROTECT_DICT(x_dict, &n_prot);

  struct dictionary* y_dict = new_dictionary_partial(y_proxy);
  PROTECT_DICT(y_dict, &n_prot);

  r_obj* marked_shelter = KEEP_N(r_alloc_raw(x_size * sizeof(bool)), &n_prot);
  bool* v_marked = (bool*) r_raw_begin(marked_shelter);
  r_memset(v_marked, 0, x_size * sizeof(bool));

  vec_set_intersect_loop(x_dict, y_dict, x_size, y_size, v_marked);

  r_ssize n_marked = 0;
  for (r_ssize i = 0; i < x_size; ++i) {
    n_marked += v_marked[i];
  }

  r_obj* loc = KEEP_N(r_alloc_integer(n_marked), &n_prot);
  int* v_loc = r_int_begin(loc);
  r_ssize j = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    if (v_marked[i]) {
      v_loc[j] = i + 1;
      ++j;
    }
  }

  r_obj* out = vec_slice_unsafe(x, loc);

  FREE(n_prot);
  return out;
}

#define VEC_SET_INTERSECT_LOOP(DICT_HASH_SCALAR, DICT_HASH_WITH)      \
do {                                                                  \
  /* Load dictionary with `x`. */                                     \
  /* Key values point to first time we saw that `x` value. */         \
  for (r_ssize i = 0; i < x_size; ++i) {                              \
    const uint32_t hash = DICT_HASH_SCALAR(x_dict, i);                \
                                                                      \
    if (x_dict->key[hash] == DICT_EMPTY) {                            \
      dict_put(x_dict, hash, i);                                      \
    }                                                                 \
  }                                                                   \
                                                                      \
  /* Mark unique elements of `x` that are also in `y` */              \
  for (r_ssize i = 0; i < y_size; ++i) {                              \
    const uint32_t hash = DICT_HASH_WITH(x_dict, y_dict, i);          \
    const r_ssize loc = x_dict->key[hash];                            \
                                                                      \
    if (loc != DICT_EMPTY) {                                          \
      v_marked[loc] = true;                                           \
    }                                                                 \
  }                                                                   \
}                                                                     \
while (0)

static inline
void vec_set_intersect_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_marked
) {
  switch (x_dict->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_SET_INTERSECT_LOOP(nil_dict_hash_scalar, nil_dict_hash_with); break;
  case VCTRS_TYPE_logical: VEC_SET_INTERSECT_LOOP(lgl_dict_hash_scalar, lgl_dict_hash_with); break;
  case VCTRS_TYPE_integer: VEC_SET_INTERSECT_LOOP(int_dict_hash_scalar, int_dict_hash_with); break;
  case VCTRS_TYPE_double: VEC_SET_INTERSECT_LOOP(dbl_dict_hash_scalar, dbl_dict_hash_with); break;
  case VCTRS_TYPE_complex: VEC_SET_INTERSECT_LOOP(cpl_dict_hash_scalar, cpl_dict_hash_with); break;
  case VCTRS_TYPE_character: VEC_SET_INTERSECT_LOOP(chr_dict_hash_scalar, chr_dict_hash_with); break;
  case VCTRS_TYPE_raw: VEC_SET_INTERSECT_LOOP(raw_dict_hash_scalar, raw_dict_hash_with); break;
  case VCTRS_TYPE_list: VEC_SET_INTERSECT_LOOP(list_dict_hash_scalar, list_dict_hash_with); break;
  case VCTRS_TYPE_dataframe: VEC_SET_INTERSECT_LOOP(df_dict_hash_scalar, df_dict_hash_with); break;
  default: stop_unimplemented_vctrs_type("vec_set_intersect_loop", x_dict->p_poly_vec->type);
  }
}

#undef VEC_SET_INTERSECT_LOOP

// -----------------------------------------------------------------------------

r_obj* ffi_vec_set_difference(r_obj* x,
                              r_obj* y,
                              r_obj* ptype,
                              r_obj* frame) {
  struct r_lazy call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_lazy = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_lazy);

  return vec_set_difference(x, y, ptype, &x_arg, &y_arg, call);
}

r_obj* vec_set_difference(r_obj* x,
                          r_obj* y,
                          r_obj* ptype,
                          struct vctrs_arg* x_arg,
                          struct vctrs_arg* y_arg,
                          struct r_lazy call) {
  int n_prot = 0;

  if (ptype == r_null) {
    int _;

    ptype = vec_ptype2(
      x,
      y,
      x_arg,
      y_arg,
      call,
      S3_FALLBACK_false,
      &_
    );
    KEEP_N(ptype, &n_prot);

    ptype = vec_ptype_finalise(ptype);
    KEEP_N(ptype, &n_prot);
  }

  x = vec_cast_params(
    x,
    ptype,
    x_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(x, &n_prot);

  y = vec_cast_params(
    y,
    ptype,
    y_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(y, &n_prot);

  r_obj* x_proxy = KEEP_N(vec_proxy_equal(x), &n_prot);
  x_proxy = KEEP_N(vec_normalize_encoding(x_proxy), &n_prot);

  r_obj* y_proxy = KEEP_N(vec_proxy_equal(y), &n_prot);
  y_proxy = KEEP_N(vec_normalize_encoding(y_proxy), &n_prot);

  const r_ssize x_size = vec_size(x_proxy);
  const r_ssize y_size = vec_size(y_proxy);

  struct dictionary* x_dict = new_dictionary(x_proxy);
  PROTECT_DICT(x_dict, &n_prot);

  struct dictionary* y_dict = new_dictionary_partial(y_proxy);
  PROTECT_DICT(y_dict, &n_prot);

  r_obj* marked_shelter = KEEP_N(r_alloc_raw(x_size * sizeof(bool)), &n_prot);
  bool* v_marked = (bool*) r_raw_begin(marked_shelter);

  vec_set_difference_loop(x_dict, y_dict, x_size, y_size, v_marked);

  r_ssize n_marked = 0;
  for (r_ssize i = 0; i < x_size; ++i) {
    n_marked += v_marked[i];
  }

  r_obj* loc = KEEP_N(r_alloc_integer(n_marked), &n_prot);
  int* v_loc = r_int_begin(loc);
  r_ssize j = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    if (v_marked[i]) {
      v_loc[j] = i + 1;
      ++j;
    }
  }

  r_obj* out = vec_slice_unsafe(x, loc);

  FREE(n_prot);
  return out;
}

#define VEC_SET_DIFFERENCE_LOOP(DICT_HASH_SCALAR, DICT_HASH_WITH)     \
do {                                                                  \
  /* Load dictionary with `x`. */                                     \
  /* Key values point to first time we saw that `x` value. */         \
  /* Mark those first seen locations as potential results. */         \
  for (r_ssize i = 0; i < x_size; ++i) {                              \
    const uint32_t hash = DICT_HASH_SCALAR(x_dict, i);                \
    const bool first_time = x_dict->key[hash] == DICT_EMPTY;          \
                                                                      \
    if (first_time) {                                                 \
      dict_put(x_dict, hash, i);                                      \
    }                                                                 \
                                                                      \
    v_marked[i] = first_time;                                         \
  }                                                                   \
                                                                      \
  /* If we've seen the `y` element in `x`, unmark it */               \
  for (r_ssize i = 0; i < y_size; ++i) {                              \
    const uint32_t hash = DICT_HASH_WITH(x_dict, y_dict, i);          \
    const r_ssize loc = x_dict->key[hash];                            \
                                                                      \
    if (loc != DICT_EMPTY) {                                          \
      v_marked[loc] = false;                                          \
    }                                                                 \
  }                                                                   \
}                                                                     \
while (0)

static inline
void vec_set_difference_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_marked
) {
  switch (x_dict->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_SET_DIFFERENCE_LOOP(nil_dict_hash_scalar, nil_dict_hash_with); break;
  case VCTRS_TYPE_logical: VEC_SET_DIFFERENCE_LOOP(lgl_dict_hash_scalar, lgl_dict_hash_with); break;
  case VCTRS_TYPE_integer: VEC_SET_DIFFERENCE_LOOP(int_dict_hash_scalar, int_dict_hash_with); break;
  case VCTRS_TYPE_double: VEC_SET_DIFFERENCE_LOOP(dbl_dict_hash_scalar, dbl_dict_hash_with); break;
  case VCTRS_TYPE_complex: VEC_SET_DIFFERENCE_LOOP(cpl_dict_hash_scalar, cpl_dict_hash_with); break;
  case VCTRS_TYPE_character: VEC_SET_DIFFERENCE_LOOP(chr_dict_hash_scalar, chr_dict_hash_with); break;
  case VCTRS_TYPE_raw: VEC_SET_DIFFERENCE_LOOP(raw_dict_hash_scalar, raw_dict_hash_with); break;
  case VCTRS_TYPE_list: VEC_SET_DIFFERENCE_LOOP(list_dict_hash_scalar, list_dict_hash_with); break;
  case VCTRS_TYPE_dataframe: VEC_SET_DIFFERENCE_LOOP(df_dict_hash_scalar, df_dict_hash_with); break;
  default: stop_unimplemented_vctrs_type("vec_set_difference_loop", x_dict->p_poly_vec->type);
  }
}

#undef VEC_SET_DIFFERENCE_LOOP

// -----------------------------------------------------------------------------

r_obj* ffi_vec_set_union(r_obj* x,
                         r_obj* y,
                         r_obj* ptype,
                         r_obj* frame) {
  struct r_lazy call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_lazy = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_lazy);

  return vec_set_union(x, y, ptype, &x_arg, &y_arg, call);
}

r_obj* vec_set_union(r_obj* x,
                     r_obj* y,
                     r_obj* ptype,
                     struct vctrs_arg* x_arg,
                     struct vctrs_arg* y_arg,
                     struct r_lazy call) {
  int n_prot = 0;

  if (ptype == r_null) {
    int _;

    ptype = vec_ptype2(
      x,
      y,
      x_arg,
      y_arg,
      call,
      S3_FALLBACK_false,
      &_
    );
    KEEP_N(ptype, &n_prot);

    ptype = vec_ptype_finalise(ptype);
    KEEP_N(ptype, &n_prot);
  }

  x = vec_cast_params(
    x,
    ptype,
    x_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(x, &n_prot);

  y = vec_cast_params(
    y,
    ptype,
    y_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(y, &n_prot);

  r_obj* x_proxy = KEEP_N(vec_proxy_equal(x), &n_prot);
  x_proxy = KEEP_N(vec_normalize_encoding(x_proxy), &n_prot);

  r_obj* y_proxy = KEEP_N(vec_proxy_equal(y), &n_prot);
  y_proxy = KEEP_N(vec_normalize_encoding(y_proxy), &n_prot);

  const r_ssize x_size = vec_size(x_proxy);
  const r_ssize y_size = vec_size(y_proxy);

  struct dictionary* x_dict = new_dictionary(x_proxy);
  PROTECT_DICT(x_dict, &n_prot);

  struct dictionary* y_dict = new_dictionary(y_proxy);
  PROTECT_DICT(y_dict, &n_prot);

  r_obj* marked_shelter = KEEP_N(r_alloc_raw(x_size * sizeof(bool)), &n_prot);
  bool* v_marked = (bool*) r_raw_begin(marked_shelter);

  const r_ssize n_x_marked = vec_set_union_x_loop(
    x_dict,
    x_size,
    v_marked
  );

  r_obj* loc = KEEP_N(r_alloc_integer(n_x_marked), &n_prot);
  int* v_loc = r_int_begin(loc);
  r_ssize j = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    if (v_marked[i]) {
      v_loc[j] = i + 1;
      ++j;
    }
  }

  // Go ahead and slice out `x`
  x = KEEP_N(vec_slice_unsafe(x, loc), &n_prot);

  // Resize `v_marked` for use with `y`
  marked_shelter = KEEP_N(r_raw_resize(marked_shelter, y_size * sizeof(bool)), &n_prot);
  v_marked = (bool*) r_raw_begin(marked_shelter);

  const r_ssize n_y_marked = vec_set_union_y_loop(
    x_dict,
    y_dict,
    x_size,
    y_size,
    v_marked
  );

  loc = KEEP_N(r_int_resize(loc, n_y_marked), &n_prot);
  v_loc = r_int_begin(loc);
  j = 0;

  for (r_ssize i = 0; i < y_size; ++i) {
    if (v_marked[i]) {
      v_loc[j] = i + 1;
      ++j;
    }
  }

  y = KEEP_N(vec_slice_unsafe(y, loc), &n_prot);

  const struct name_repair_opts name_repair_opts = {
    .type = NAME_REPAIR_none,
    .fn = r_null
  };

  r_obj* args = KEEP_N(r_alloc_list(2), &n_prot);
  r_list_poke(args, 0, x);
  r_list_poke(args, 1, y);

  r_obj* out = vec_c(
    args,
    ptype,
    r_null,
    &name_repair_opts,
    vec_args.empty,
    r_lazy_null
  );

  FREE(n_prot);
  return out;
}

#define VEC_SET_UNION_X_LOOP(DICT_HASH_SCALAR)                    \
do {                                                              \
  /* Load dictionary with `x`. */                                 \
  /* Key values point to first time we saw that `x` value. */     \
  /* Mark those first seen locations as definite results. */      \
  for (r_ssize i = 0; i < x_size; ++i) {                          \
    const uint32_t hash = DICT_HASH_SCALAR(x_dict, i);            \
    const bool first_time = x_dict->key[hash] == DICT_EMPTY;      \
                                                                  \
    if (first_time) {                                             \
      dict_put(x_dict, hash, i);                                  \
    }                                                             \
                                                                  \
    v_marked[i] = first_time;                                     \
  }                                                               \
                                                                  \
  return x_dict->used;                                            \
}                                                                 \
while (0)

static inline
r_ssize vec_set_union_x_loop(
  struct dictionary* x_dict,
  r_ssize x_size,
  bool* v_marked
) {
  switch (x_dict->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_SET_UNION_X_LOOP(nil_dict_hash_scalar);
  case VCTRS_TYPE_logical: VEC_SET_UNION_X_LOOP(lgl_dict_hash_scalar);
  case VCTRS_TYPE_integer: VEC_SET_UNION_X_LOOP(int_dict_hash_scalar);
  case VCTRS_TYPE_double: VEC_SET_UNION_X_LOOP(dbl_dict_hash_scalar);
  case VCTRS_TYPE_complex: VEC_SET_UNION_X_LOOP(cpl_dict_hash_scalar);
  case VCTRS_TYPE_character: VEC_SET_UNION_X_LOOP(chr_dict_hash_scalar);
  case VCTRS_TYPE_raw: VEC_SET_UNION_X_LOOP(raw_dict_hash_scalar);
  case VCTRS_TYPE_list: VEC_SET_UNION_X_LOOP(list_dict_hash_scalar);
  case VCTRS_TYPE_dataframe: VEC_SET_UNION_X_LOOP(df_dict_hash_scalar);
  default: stop_unimplemented_vctrs_type("vec_set_union_x_loop", x_dict->p_poly_vec->type);
  }
}

#undef VEC_SET_UNION_X_LOOP

#define VEC_SET_UNION_Y_LOOP(DICT_HASH_SCALAR, DICT_HASH_WITH)     \
do {                                                               \
  /* Load dictionary with `y`. */                                  \
  /* Key values point to first time we saw that `y` value. */      \
  /* Mark those first seen locations as possible results. */       \
  for (r_ssize i = 0; i < y_size; ++i) {                           \
    const uint32_t hash = DICT_HASH_SCALAR(y_dict, i);             \
    const bool first_time = y_dict->key[hash] == DICT_EMPTY;       \
                                                                   \
    if (first_time) {                                              \
      dict_put(y_dict, hash, i);                                   \
    }                                                              \
                                                                   \
    v_marked[i] = first_time;                                      \
  }                                                                \
                                                                   \
  r_ssize n_marked = y_dict->used;                                 \
                                                                   \
  /* Check if unique elements of `y` are in `x`. */                \
  /* If they are, unmark them. */                                  \
  for (r_ssize i = 0; i < y_size; ++i) {                           \
    if (!v_marked[i]) {                                            \
      continue;                                                    \
    }                                                              \
                                                                   \
    const uint32_t hash = DICT_HASH_WITH(x_dict, y_dict, i);       \
    const bool in_x = x_dict->key[hash] != DICT_EMPTY;             \
                                                                   \
    v_marked[i] = !in_x;                                           \
    n_marked -= in_x;                                              \
  }                                                                \
                                                                   \
  return n_marked;                                                 \
}                                                                  \
while (0)

static inline
r_ssize vec_set_union_y_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_marked
) {
  switch (x_dict->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_SET_UNION_Y_LOOP(nil_dict_hash_scalar, nil_dict_hash_with);
  case VCTRS_TYPE_logical: VEC_SET_UNION_Y_LOOP(lgl_dict_hash_scalar, lgl_dict_hash_with);
  case VCTRS_TYPE_integer: VEC_SET_UNION_Y_LOOP(int_dict_hash_scalar, int_dict_hash_with);
  case VCTRS_TYPE_double: VEC_SET_UNION_Y_LOOP(dbl_dict_hash_scalar, dbl_dict_hash_with);
  case VCTRS_TYPE_complex: VEC_SET_UNION_Y_LOOP(cpl_dict_hash_scalar, cpl_dict_hash_with);
  case VCTRS_TYPE_character: VEC_SET_UNION_Y_LOOP(chr_dict_hash_scalar, chr_dict_hash_with);
  case VCTRS_TYPE_raw: VEC_SET_UNION_Y_LOOP(raw_dict_hash_scalar, raw_dict_hash_with);
  case VCTRS_TYPE_list: VEC_SET_UNION_Y_LOOP(list_dict_hash_scalar, list_dict_hash_with);
  case VCTRS_TYPE_dataframe: VEC_SET_UNION_Y_LOOP(df_dict_hash_scalar, df_dict_hash_with);
  default: stop_unimplemented_vctrs_type("vec_set_union_y_loop", x_dict->p_poly_vec->type);
  }
}

#undef VEC_SET_UNION_Y_LOOP

// -----------------------------------------------------------------------------

r_obj* ffi_vec_set_symmetric_difference(r_obj* x,
                                        r_obj* y,
                                        r_obj* ptype,
                                        r_obj* frame) {
  struct r_lazy call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_lazy = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_lazy);

  return vec_set_symmetric_difference(x, y, ptype, &x_arg, &y_arg, call);
}

r_obj* vec_set_symmetric_difference(r_obj* x,
                                    r_obj* y,
                                    r_obj* ptype,
                                    struct vctrs_arg* x_arg,
                                    struct vctrs_arg* y_arg,
                                    struct r_lazy call) {
  int n_prot = 0;

  if (ptype == r_null) {
    int _;

    ptype = vec_ptype2(
      x,
      y,
      x_arg,
      y_arg,
      call,
      S3_FALLBACK_false,
      &_
    );
    KEEP_N(ptype, &n_prot);

    ptype = vec_ptype_finalise(ptype);
    KEEP_N(ptype, &n_prot);
  }

  x = vec_cast_params(
    x,
    ptype,
    x_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(x, &n_prot);

  y = vec_cast_params(
    y,
    ptype,
    y_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(y, &n_prot);

  r_obj* x_proxy = KEEP_N(vec_proxy_equal(x), &n_prot);
  x_proxy = KEEP_N(vec_normalize_encoding(x_proxy), &n_prot);

  r_obj* y_proxy = KEEP_N(vec_proxy_equal(y), &n_prot);
  y_proxy = KEEP_N(vec_normalize_encoding(y_proxy), &n_prot);

  const r_ssize x_size = vec_size(x_proxy);
  const r_ssize y_size = vec_size(y_proxy);

  struct dictionary* x_dict = new_dictionary(x_proxy);
  PROTECT_DICT(x_dict, &n_prot);

  struct dictionary* y_dict = new_dictionary(y_proxy);
  PROTECT_DICT(y_dict, &n_prot);

  r_obj* x_marked_shelter = KEEP_N(r_alloc_raw(x_size * sizeof(bool)), &n_prot);
  bool* v_x_marked = (bool*) r_raw_begin(x_marked_shelter);

  r_obj* y_marked_shelter = KEEP_N(r_alloc_raw(y_size * sizeof(bool)), &n_prot);
  bool* v_y_marked = (bool*) r_raw_begin(y_marked_shelter);

  const struct r_ssize_pair n_marked = vec_set_symmetric_difference_loop(
    x_dict,
    y_dict,
    x_size,
    y_size,
    v_x_marked,
    v_y_marked
  );

  const r_ssize n_x_marked = n_marked.x;
  const r_ssize n_y_marked = n_marked.y;

  r_obj* loc = KEEP_N(r_alloc_integer(n_x_marked), &n_prot);
  int* v_loc = r_int_begin(loc);
  r_ssize j = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    if (v_x_marked[i]) {
      v_loc[j] = i + 1;
      ++j;
    }
  }

  // Slice out `x`, then reuse `loc` for slicing `y`
  x = KEEP_N(vec_slice_unsafe(x, loc), &n_prot);

  loc = KEEP_N(r_int_resize(loc, n_y_marked), &n_prot);
  v_loc = r_int_begin(loc);
  j = 0;

  for (r_ssize i = 0; i < y_size; ++i) {
    if (v_y_marked[i]) {
      v_loc[j] = i + 1;
      ++j;
    }
  }

  y = KEEP_N(vec_slice_unsafe(y, loc), &n_prot);

  const struct name_repair_opts name_repair_opts = {
    .type = NAME_REPAIR_none,
    .fn = r_null
  };

  r_obj* args = KEEP_N(r_alloc_list(2), &n_prot);
  r_list_poke(args, 0, x);
  r_list_poke(args, 1, y);

  r_obj* out = vec_c(
    args,
    ptype,
    r_null,
    &name_repair_opts,
    vec_args.empty,
    r_lazy_null
  );

  FREE(n_prot);
  return out;
}

#define VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(DICT_HASH_SCALAR, DICT_HASH_WITH)     \
do {                                                                            \
  /* Load dictionary with `x`. */                                               \
  /* Key values point to first time we saw that `x` value. */                   \
  /* Mark those first seen locations as possible results. */                    \
  for (r_ssize i = 0; i < x_size; ++i) {                                        \
    const uint32_t hash = DICT_HASH_SCALAR(x_dict, i);                          \
    const bool first_time = x_dict->key[hash] == DICT_EMPTY;                    \
                                                                                \
    if (first_time) {                                                           \
      dict_put(x_dict, hash, i);                                                \
    }                                                                           \
                                                                                \
    v_x_marked[i] = first_time;                                                 \
  }                                                                             \
                                                                                \
  /* Load dictionary with `y`. */                                               \
  /* Key values point to first time we saw that `y` value. */                   \
  /* Mark those first seen locations as possible results. */                    \
  for (r_ssize i = 0; i < y_size; ++i) {                                        \
    const uint32_t hash = DICT_HASH_SCALAR(y_dict, i);                          \
    const bool first_time = y_dict->key[hash] == DICT_EMPTY;                    \
                                                                                \
    if (first_time) {                                                           \
      dict_put(y_dict, hash, i);                                                \
    }                                                                           \
                                                                                \
    v_y_marked[i] = first_time;                                                 \
  }                                                                             \
                                                                                \
  r_ssize n_x_marked = x_dict->used;                                            \
  r_ssize n_y_marked = y_dict->used;                                            \
                                                                                \
  /* Check if unique elements of `y` are in `x`. */                             \
  /* If they are, unmark them from both `x` and `y`. */                         \
  for (r_ssize i = 0; i < y_size; ++i) {                                        \
    if (!v_y_marked[i]) {                                                       \
      continue;                                                                 \
    }                                                                           \
                                                                                \
    const uint32_t hash = DICT_HASH_WITH(x_dict, y_dict, i);                    \
    const r_ssize loc = x_dict->key[hash];                                      \
    const bool in_x = loc != DICT_EMPTY;                                        \
                                                                                \
    if (in_x) {                                                                 \
      v_x_marked[loc] = false;                                                  \
      v_y_marked[i] = false;                                                    \
      --n_x_marked;                                                             \
      --n_y_marked;                                                             \
    }                                                                           \
  }                                                                             \
                                                                                \
  struct r_ssize_pair n_marked = {                                              \
    .x = n_x_marked,                                                            \
    .y = n_y_marked                                                             \
  };                                                                            \
                                                                                \
  return n_marked;                                                              \
}                                                                               \
while (0)

static inline
struct r_ssize_pair vec_set_symmetric_difference_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_x_marked,
  bool* v_y_marked
) {
  switch (x_dict->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(nil_dict_hash_scalar, nil_dict_hash_with); break;
  case VCTRS_TYPE_logical: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(lgl_dict_hash_scalar, lgl_dict_hash_with); break;
  case VCTRS_TYPE_integer: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(int_dict_hash_scalar, int_dict_hash_with); break;
  case VCTRS_TYPE_double: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(dbl_dict_hash_scalar, dbl_dict_hash_with); break;
  case VCTRS_TYPE_complex: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(cpl_dict_hash_scalar, cpl_dict_hash_with); break;
  case VCTRS_TYPE_character: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(chr_dict_hash_scalar, chr_dict_hash_with); break;
  case VCTRS_TYPE_raw: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(raw_dict_hash_scalar, raw_dict_hash_with); break;
  case VCTRS_TYPE_list: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(list_dict_hash_scalar, list_dict_hash_with); break;
  case VCTRS_TYPE_dataframe: VEC_SET_SYMMETRIC_DIFFERENCE_LOOP(df_dict_hash_scalar, df_dict_hash_with); break;
  default: stop_unimplemented_vctrs_type("vec_set_symmetric_difference_loop", x_dict->p_poly_vec->type);
  }
}

#undef VEC_SET_SYMMETRIC_DIFFERENCE_LOOP
