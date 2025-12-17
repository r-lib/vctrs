#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/dictionary-decl.h"

// Initialised at load time
struct vctrs_arg args_needles;
struct vctrs_arg args_haystack;


// http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
static inline
uint32_t u32_safe_ceil2(uint32_t x) {
  // Return 2^0 when `x` is 0
  x += (x == 0);

  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;

  if (x == 0) {
    // INT32_MAX+2 <= x <= UINT32_MAX (i.e. 2^31+1 <= x <= 2^32-1) would attempt
    // to ceiling to 2^32, which is 1 greater than `UINT32_MAX`, resulting in
    // overflow wraparound to 0.
    r_stop_internal("`x` results in an `uint32_t` overflow.");
  }

  return x;
}

// Dictonary object ------------------------------------------------------------

static struct dictionary* new_dictionary_opts(SEXP x, struct dictionary_opts* opts);

// Dictionaries must be protected in consistent stack order with
// `PROTECT_DICT()`
struct dictionary* new_dictionary(SEXP x) {
  struct dictionary_opts opts = {
    .partial = false,
    .na_equal = true
  };
  return new_dictionary_opts(x, &opts);
}
struct dictionary* new_dictionary_partial(SEXP x) {
  struct dictionary_opts opts = {
    .partial = true,
    .na_equal = true
  };
  return new_dictionary_opts(x, &opts);
}

static struct dictionary* new_dictionary_params(SEXP x, bool partial, bool na_equal) {
  struct dictionary_opts opts;
  opts.partial = partial;
  opts.na_equal = na_equal;
  return new_dictionary_opts(x, &opts);
}

static struct dictionary* new_dictionary_opts(SEXP x, struct dictionary_opts* opts) {
  int nprot = 0;

  SEXP out = PROTECT_N(Rf_allocVector(RAWSXP, sizeof(struct dictionary)), &nprot);
  struct dictionary* d = (struct dictionary*) RAW(out);

  d->protect = out;

  enum vctrs_type type = vec_proxy_typeof(x);

  struct poly_vec* p_poly_vec = new_poly_vec(x, type);
  KEEP_N(p_poly_vec->shelter, &nprot);
  d->p_poly_vec = p_poly_vec;

  d->used = 0;

  if (opts->partial) {
    d->key = NULL;
    d->size = 0;
  } else {
    uint32_t size = dict_key_size(x);

    d->key = (R_len_t*) R_alloc(size, sizeof(R_len_t));

    for (uint32_t i = 0; i < size; ++i) {
      d->key[i] = DICT_EMPTY;
    }

    d->size = size;
  }

  R_len_t n = vec_size(x);
  if (n) {
    d->hash = (uint32_t*) R_alloc(n, sizeof(uint32_t));

    if (!(d->hash)) {
      Rf_errorcall(R_NilValue, "Can't allocate hash lookup table. Please free memory.");
    }

    r_memset(d->hash, 0, n * sizeof(uint32_t));
    vec_hash_fill(x, n, opts->na_equal, d->hash);
  } else {
    d->hash = NULL;
  }

  UNPROTECT(nprot);
  return d;
}

// Assume worst case, that every value is distinct, aiming for a load factor
// of at most 50%. We round up to power of 2 to ensure quadratic probing
// strategy works. Maximum power of 2 we can store in a uint32_t is 2^31,
// as 2^32 is 1 greater than the max uint32_t value, so we clamp sizes that
// would result in 2^32 to INT32_MAX to ensure that our maximum ceiling value
// is only 2^31. This will increase the max load factor above 50% for `x` with
// length greater than 1073741824 (2147483648 * .50), but it ensures that
// it can run. See https://github.com/r-lib/vctrs/pull/1760 for further
// discussion of why 50% was chosen.
static inline
uint32_t dict_key_size(SEXP x) {
  const R_len_t x_size = vec_size(x);

  if (x_size > R_LEN_T_MAX) {
    // Ensure we catch the switch to supporting long vectors in `vec_size()`
    r_stop_internal("Dictionary functions do not support long vectors.");
  }

  const double load_adjusted_size = x_size / 0.50;

  if (load_adjusted_size > UINT32_MAX) {
    r_stop_internal("Can't safely cast load adjusted size to a `uint32_t`.");
  }

  uint32_t size = (uint32_t)load_adjusted_size;
  // Clamp to `INT32_MAX` to avoid overflow in `u32_safe_ceil2()`,
  // at the cost of an increased maximum load factor for long input
  size = size > INT32_MAX ? INT32_MAX : size;
  size = u32_safe_ceil2(size);
  size = (size < 16) ? 16 : size;

  if (x_size > size) {
    // Should never happen with `R_len_t` sizes.
    // This is a defensive check that will be useful when we support long vectors.
    r_stop_internal("Hash table size must be at least as large as input to avoid a load factor of >100%.");
  }

  // Rprintf("size: %u\n", size);
  return size;
}

// R interface -----------------------------------------------------------------
// TODO: rename to match R function names
// TODO: separate out into individual files

SEXP vctrs_unique_loc(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  struct growable g = new_growable(INTSXP, 256);
  PROTECT_GROWABLE(&g, &nprot);

  vctrs_unique_loc_loop(d, &g, n);

  SEXP out = growable_values(&g);

  UNPROTECT(nprot);
  return out;
}

#define VCTRS_UNIQUE_LOC_LOOP(DICT_HASH_WITH)   \
do {                                            \
  for (int i = 0; i < n; ++i) {                 \
    uint32_t hash = DICT_HASH_WITH(d, d, i);    \
                                                \
    if (d->key[hash] == DICT_EMPTY) {           \
      dict_put(d, hash, i);                     \
      growable_push_int(g, i + 1);              \
    }                                           \
  }                                             \
}                                               \
while (0)

static inline
void vctrs_unique_loc_loop(struct dictionary* d, struct growable* g, R_len_t n) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VCTRS_UNIQUE_LOC_LOOP(nil_dict_hash_with); break;
  case VCTRS_TYPE_logical: VCTRS_UNIQUE_LOC_LOOP(lgl_dict_hash_with); break;
  case VCTRS_TYPE_integer: VCTRS_UNIQUE_LOC_LOOP(int_dict_hash_with); break;
  case VCTRS_TYPE_double: VCTRS_UNIQUE_LOC_LOOP(dbl_dict_hash_with); break;
  case VCTRS_TYPE_complex: VCTRS_UNIQUE_LOC_LOOP(cpl_dict_hash_with); break;
  case VCTRS_TYPE_character: VCTRS_UNIQUE_LOC_LOOP(chr_dict_hash_with); break;
  case VCTRS_TYPE_raw: VCTRS_UNIQUE_LOC_LOOP(raw_dict_hash_with); break;
  case VCTRS_TYPE_list: VCTRS_UNIQUE_LOC_LOOP(list_dict_hash_with); break;
  case VCTRS_TYPE_dataframe: VCTRS_UNIQUE_LOC_LOOP(df_dict_hash_with); break;
  default: stop_unimplemented_vctrs_type("vctrs_unique_loc_loop", d->p_poly_vec->type);
  }
}

#undef VCTRS_UNIQUE_LOC_LOOP

// [[ include("vctrs.h") ]]
SEXP vec_unique(SEXP x) {
  SEXP index = PROTECT(vctrs_unique_loc(x));
  SEXP out = vec_slice_unsafe(x, index);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_duplicated_any(SEXP x) {
  bool out = duplicated_any(x);
  return Rf_ScalarLogical(out);
}

// [[ include("vctrs.h") ]]
bool duplicated_any(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  bool out = duplicated_any_loop(d, n);

  UNPROTECT(nprot);
  return out;
}

#define DUPLICATED_ANY_LOOP(DICT_HASH_SCALAR) \
do {                                          \
  for (int i = 0; i < n; ++i) {               \
    uint32_t hash = DICT_HASH_SCALAR(d, i);   \
                                              \
    if (d->key[hash] == DICT_EMPTY) {         \
      dict_put(d, hash, i);                   \
    } else {                                  \
      return true;                            \
    }                                         \
  }                                           \
                                              \
  return false;                               \
}                                             \
while (0)

static inline
bool duplicated_any_loop(struct dictionary* d, R_len_t n) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: DUPLICATED_ANY_LOOP(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: DUPLICATED_ANY_LOOP(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: DUPLICATED_ANY_LOOP(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: DUPLICATED_ANY_LOOP(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: DUPLICATED_ANY_LOOP(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: DUPLICATED_ANY_LOOP(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: DUPLICATED_ANY_LOOP(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: DUPLICATED_ANY_LOOP(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: DUPLICATED_ANY_LOOP(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("duplicated_any_loop", d->p_poly_vec->type);
  }
}

#undef DUPLICATED_ANY_LOOP

SEXP vctrs_n_distinct(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  vctrs_n_distinct_loop(d, n);

  UNPROTECT(nprot);
  return Rf_ScalarInteger(d->used);
}

#define VCTRS_N_DISTINCT_LOOP(DICT_HASH_SCALAR) \
do {                                            \
  for (int i = 0; i < n; ++i) {                 \
    uint32_t hash = DICT_HASH_SCALAR(d, i);     \
                                                \
    if (d->key[hash] == DICT_EMPTY) {           \
      dict_put(d, hash, i);                     \
    }                                           \
  }                                             \
}                                               \
while (0)

static inline
void vctrs_n_distinct_loop(struct dictionary* d, R_len_t n) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VCTRS_N_DISTINCT_LOOP(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: VCTRS_N_DISTINCT_LOOP(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: VCTRS_N_DISTINCT_LOOP(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: VCTRS_N_DISTINCT_LOOP(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: VCTRS_N_DISTINCT_LOOP(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: VCTRS_N_DISTINCT_LOOP(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: VCTRS_N_DISTINCT_LOOP(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: VCTRS_N_DISTINCT_LOOP(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: VCTRS_N_DISTINCT_LOOP(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("vctrs_n_distinct_loop", d->p_poly_vec->type);
  }
}

#undef VCTRS_N_DISTINCT_LOOP

SEXP vctrs_id(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out = INTEGER(out);

  vctrs_id_loop(d, n, p_out);

  UNPROTECT(nprot);
  return out;
}

#define VCTRS_ID_LOOP(DICT_HASH_SCALAR)       \
do {                                          \
  for (int i = 0; i < n; ++i) {               \
    uint32_t hash = DICT_HASH_SCALAR(d, i);   \
                                              \
    if (d->key[hash] == DICT_EMPTY) {         \
      dict_put(d, hash, i);                   \
    }                                         \
                                              \
    p_out[i] = d->key[hash] + 1;              \
  }                                           \
}                                             \
while (0)

static inline
void vctrs_id_loop(struct dictionary* d, R_len_t n, int* p_out) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VCTRS_ID_LOOP(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: VCTRS_ID_LOOP(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: VCTRS_ID_LOOP(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: VCTRS_ID_LOOP(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: VCTRS_ID_LOOP(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: VCTRS_ID_LOOP(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: VCTRS_ID_LOOP(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: VCTRS_ID_LOOP(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: VCTRS_ID_LOOP(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("vctrs_id_loop", d->p_poly_vec->type);
  }
}

#undef VCTRS_ID_LOOP

// [[ register() ]]
SEXP vctrs_match(SEXP needles, SEXP haystack, SEXP na_equal,
                 SEXP frame) {
  struct r_lazy call = { .x = frame, .env = r_null };

  struct r_lazy needles_arg_ = { .x = syms.needles_arg, .env = frame };
  struct vctrs_arg needles_arg = new_lazy_arg(&needles_arg_);

  struct r_lazy haystack_arg_ = { .x = syms.haystack_arg, .env = frame };
  struct vctrs_arg haystack_arg = new_lazy_arg(&haystack_arg_);

  return vec_match_params(needles,
                          haystack,
                          r_bool_as_int(na_equal),
                          &needles_arg,
                          &haystack_arg,
                          call);
}

SEXP vec_match_params(SEXP needles,
                      SEXP haystack,
                      bool na_equal,
                      struct vctrs_arg* needles_arg,
                      struct vctrs_arg* haystack_arg,
                      struct r_lazy call) {
  int nprot = 0;

  int _;
  SEXP type = vec_ptype2(
    needles,
    haystack,
    needles_arg,
    haystack_arg,
    call,
    S3_FALLBACK_false,
    &_
  );
  PROTECT_N(type, &nprot);
  type = PROTECT_N(vec_ptype_finalise(type), &nprot);

  needles = vec_cast_params(
    needles,
    type,
    needles_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  PROTECT_N(needles, &nprot);

  haystack = vec_cast_params(
    haystack,
    type,
    haystack_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  PROTECT_N(haystack, &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  needles = PROTECT_N(vec_normalize_encoding(needles), &nprot);

  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);
  haystack = PROTECT_N(vec_normalize_encoding(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  struct dictionary* d = new_dictionary_params(haystack, false, na_equal);
  PROTECT_DICT(d, &nprot);

  // Load dictionary with haystack
  load_with_haystack(d, n_haystack);

  struct dictionary* d_needles = new_dictionary_params(needles, true, na_equal);
  PROTECT_DICT(d_needles, &nprot);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n_needle), &nprot);
  int* p_out = INTEGER(out);

  if (na_equal) {
    vec_match_loop(p_out, d, d_needles, n_needle);
  } else {
    vec_match_loop_propagate(p_out, d, d_needles, n_needle);
  }

  UNPROTECT(nprot);
  return out;
}

#define VEC_MATCH_LOOP(DICT_HASH_WITH)               \
do {                                                 \
  for (R_len_t i = 0; i < n_needle; ++i) {           \
    uint32_t hash = DICT_HASH_WITH(d, d_needles, i); \
                                                     \
    if (d->key[hash] == DICT_EMPTY) {                \
      /* TODO: Return `no_match` instead */          \
      p_out[i] = NA_INTEGER;                         \
    } else {                                         \
      p_out[i] = d->key[hash] + 1;                   \
    }                                                \
  }                                                  \
}                                                    \
while (0)

static inline
void vec_match_loop(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_MATCH_LOOP(nil_dict_hash_with); break;
  case VCTRS_TYPE_logical: VEC_MATCH_LOOP(lgl_dict_hash_with); break;
  case VCTRS_TYPE_integer: VEC_MATCH_LOOP(int_dict_hash_with); break;
  case VCTRS_TYPE_double: VEC_MATCH_LOOP(dbl_dict_hash_with); break;
  case VCTRS_TYPE_complex: VEC_MATCH_LOOP(cpl_dict_hash_with); break;
  case VCTRS_TYPE_character: VEC_MATCH_LOOP(chr_dict_hash_with); break;
  case VCTRS_TYPE_raw: VEC_MATCH_LOOP(raw_dict_hash_with); break;
  case VCTRS_TYPE_list: VEC_MATCH_LOOP(list_dict_hash_with); break;
  case VCTRS_TYPE_dataframe: VEC_MATCH_LOOP(df_dict_hash_with); break;
  default: stop_unimplemented_vctrs_type("vec_match_loop", d->p_poly_vec->type);
  }
}

#undef VEC_MATCH_LOOP

#define VEC_MATCH_LOOP_PROPAGATE(DICT_HASH_WITH, DICT_IS_INCOMPLETE)  \
do {                                                                  \
  for (R_len_t i = 0; i < n_needle; ++i) {                            \
    if (DICT_IS_INCOMPLETE(d_needles, i)) {                           \
      p_out[i] = NA_INTEGER;                                          \
      continue;                                                       \
    }                                                                 \
                                                                      \
    uint32_t hash = DICT_HASH_WITH(d, d_needles, i);                  \
                                                                      \
    if (d->key[hash] == DICT_EMPTY) {                                 \
      /* TODO: Return `no_match` instead */                           \
      p_out[i] = NA_INTEGER;                                          \
    } else {                                                          \
      p_out[i] = d->key[hash] + 1;                                    \
    }                                                                 \
  }                                                                   \
}                                                                     \
while (0)

static inline
void vec_match_loop_propagate(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_MATCH_LOOP_PROPAGATE(nil_dict_hash_with, nil_dict_is_incomplete); break;
  case VCTRS_TYPE_logical: VEC_MATCH_LOOP_PROPAGATE(lgl_dict_hash_with, lgl_dict_is_incomplete); break;
  case VCTRS_TYPE_integer: VEC_MATCH_LOOP_PROPAGATE(int_dict_hash_with, int_dict_is_incomplete); break;
  case VCTRS_TYPE_double: VEC_MATCH_LOOP_PROPAGATE(dbl_dict_hash_with, dbl_dict_is_incomplete); break;
  case VCTRS_TYPE_complex: VEC_MATCH_LOOP_PROPAGATE(cpl_dict_hash_with, cpl_dict_is_incomplete); break;
  case VCTRS_TYPE_character: VEC_MATCH_LOOP_PROPAGATE(chr_dict_hash_with, chr_dict_is_incomplete); break;
  case VCTRS_TYPE_raw: VEC_MATCH_LOOP_PROPAGATE(raw_dict_hash_with, raw_dict_is_incomplete); break;
  case VCTRS_TYPE_list: VEC_MATCH_LOOP_PROPAGATE(list_dict_hash_with, list_dict_is_incomplete); break;
  case VCTRS_TYPE_dataframe: VEC_MATCH_LOOP_PROPAGATE(df_dict_hash_with, df_dict_is_incomplete); break;
  default: stop_unimplemented_vctrs_type("vec_match_loop_propagate", d->p_poly_vec->type);
  }
}

#undef VEC_MATCH_LOOP_PROPAGATE

SEXP vctrs_in(SEXP needles, SEXP haystack, SEXP na_equal_, SEXP frame) {
  struct r_lazy needles_arg_ = { .x = syms.needles_arg, .env = frame };
  struct vctrs_arg needles_arg = new_lazy_arg(&needles_arg_);

  struct r_lazy haystack_arg_ = { .x = syms.haystack_arg, .env = frame };
  struct vctrs_arg haystack_arg = new_lazy_arg(&haystack_arg_);

  struct r_lazy call = { .x = frame, .env = r_null };

  return vec_in(
    needles,
    haystack,
    r_bool_as_int(na_equal_),
    &needles_arg,
    &haystack_arg,
    call
  );
}

// [[ register() ]]
SEXP vec_in(
  SEXP needles,
  SEXP haystack,
  bool na_equal,
  struct vctrs_arg* p_needles_arg,
  struct vctrs_arg* p_haystack_arg,
  struct r_lazy call
) {
  int nprot = 0;

  int _;
  SEXP type = vec_ptype2(
    needles,
    haystack,
    p_needles_arg,
    p_haystack_arg,
    call,
    S3_FALLBACK_false,
    &_
  );
  PROTECT_N(type, &nprot);
  type = PROTECT_N(vec_ptype_finalise(type), &nprot);

  needles = vec_cast_params(
    needles,
    type,
    p_needles_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  PROTECT_N(needles, &nprot);

  haystack = vec_cast_params(
    haystack,
    type,
    p_haystack_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  PROTECT_N(haystack, &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  needles = PROTECT_N(vec_normalize_encoding(needles), &nprot);

  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);
  haystack = PROTECT_N(vec_normalize_encoding(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  struct dictionary* d = new_dictionary_params(haystack, false, na_equal);
  PROTECT_DICT(d, &nprot);

  // Load dictionary with haystack
  load_with_haystack(d, n_haystack);

  struct dictionary* d_needles = new_dictionary_params(needles, true, na_equal);
  PROTECT_DICT(d_needles, &nprot);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n_needle), &nprot);
  int* p_out = LOGICAL(out);

  if (na_equal) {
    vec_in_loop(p_out, d, d_needles, n_needle);
  } else {
    vec_in_loop_propagate(p_out, d, d_needles, n_needle);
  }

  UNPROTECT(nprot);
  return out;
}

#define VEC_IN_LOOP(DICT_HASH_WITH)                  \
do {                                                 \
  for (int i = 0; i < n_needle; ++i) {               \
    uint32_t hash = DICT_HASH_WITH(d, d_needles, i); \
    p_out[i] = (d->key[hash] != DICT_EMPTY);         \
  }                                                  \
}                                                    \
while (0)

static inline
void vec_in_loop(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_IN_LOOP(nil_dict_hash_with); break;
  case VCTRS_TYPE_logical: VEC_IN_LOOP(lgl_dict_hash_with); break;
  case VCTRS_TYPE_integer: VEC_IN_LOOP(int_dict_hash_with); break;
  case VCTRS_TYPE_double: VEC_IN_LOOP(dbl_dict_hash_with); break;
  case VCTRS_TYPE_complex: VEC_IN_LOOP(cpl_dict_hash_with); break;
  case VCTRS_TYPE_character: VEC_IN_LOOP(chr_dict_hash_with); break;
  case VCTRS_TYPE_raw: VEC_IN_LOOP(raw_dict_hash_with); break;
  case VCTRS_TYPE_list: VEC_IN_LOOP(list_dict_hash_with); break;
  case VCTRS_TYPE_dataframe: VEC_IN_LOOP(df_dict_hash_with); break;
  default: stop_unimplemented_vctrs_type("vec_in_loop", d->p_poly_vec->type);
  }
}

#undef VEC_IN_LOOP

#define VEC_IN_LOOP_PROPAGATE(DICT_HASH_WITH, DICT_IS_INCOMPLETE) \
do {                                                              \
  for (int i = 0; i < n_needle; ++i) {                            \
    if (DICT_IS_INCOMPLETE(d_needles, i)) {                       \
      p_out[i] = NA_LOGICAL;                                      \
    } else {                                                      \
      uint32_t hash = DICT_HASH_WITH(d, d_needles, i);            \
      p_out[i] = (d->key[hash] != DICT_EMPTY);                    \
    }                                                             \
  }                                                               \
}                                                                 \
while (0)

static inline
void vec_in_loop_propagate(
  int* p_out,
  struct dictionary* d,
  struct dictionary* d_needles,
  R_len_t n_needle
) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VEC_IN_LOOP_PROPAGATE(nil_dict_hash_with, nil_dict_is_incomplete); break;
  case VCTRS_TYPE_logical: VEC_IN_LOOP_PROPAGATE(lgl_dict_hash_with, lgl_dict_is_incomplete); break;
  case VCTRS_TYPE_integer: VEC_IN_LOOP_PROPAGATE(int_dict_hash_with, int_dict_is_incomplete); break;
  case VCTRS_TYPE_double: VEC_IN_LOOP_PROPAGATE(dbl_dict_hash_with, dbl_dict_is_incomplete); break;
  case VCTRS_TYPE_complex: VEC_IN_LOOP_PROPAGATE(cpl_dict_hash_with, cpl_dict_is_incomplete); break;
  case VCTRS_TYPE_character: VEC_IN_LOOP_PROPAGATE(chr_dict_hash_with, chr_dict_is_incomplete); break;
  case VCTRS_TYPE_raw: VEC_IN_LOOP_PROPAGATE(raw_dict_hash_with, raw_dict_is_incomplete); break;
  case VCTRS_TYPE_list: VEC_IN_LOOP_PROPAGATE(list_dict_hash_with, list_dict_is_incomplete); break;
  case VCTRS_TYPE_dataframe: VEC_IN_LOOP_PROPAGATE(df_dict_hash_with, df_dict_is_incomplete); break;
  default: stop_unimplemented_vctrs_type("vec_in_loop_propagate", d->p_poly_vec->type);
  }
}

#undef VEC_IN_LOOP_PROPAGATE

#define LOAD_WITH_HAYSTACK(DICT_HASH_SCALAR)      \
do {                                              \
  for (int i = 0; i < n_haystack; ++i) {          \
    uint32_t hash = DICT_HASH_SCALAR(d, i);       \
                                                  \
    if (d->key[hash] == DICT_EMPTY) {             \
      dict_put(d, hash, i);                       \
    }                                             \
  }                                               \
}                                                 \
while (0)

static inline
void load_with_haystack(struct dictionary* d, R_len_t n_haystack) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: LOAD_WITH_HAYSTACK(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: LOAD_WITH_HAYSTACK(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: LOAD_WITH_HAYSTACK(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: LOAD_WITH_HAYSTACK(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: LOAD_WITH_HAYSTACK(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: LOAD_WITH_HAYSTACK(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: LOAD_WITH_HAYSTACK(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: LOAD_WITH_HAYSTACK(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: LOAD_WITH_HAYSTACK(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("vec_match_loop", d->p_poly_vec->type);
  }
}

#undef LOAD_WITH_HAYSTACK

SEXP vctrs_count(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP count = PROTECT_N(Rf_allocVector(INTSXP, d->size), &nprot);
  int* p_count = INTEGER(count);

  // Load dictionary and accumulate `p_count`
  vctrs_count_loop(d, n, p_count);

  // Create output
  SEXP out_loc = PROTECT_N(Rf_allocVector(INTSXP, d->used), &nprot);
  int* p_out_loc = INTEGER(out_loc);

  // Reuse `count` storage, which will be narrowed
  SEXP out_count = count;
  int* p_out_count = p_count;

  int i = 0;
  for (uint32_t hash = 0; hash < d->size; ++hash) {
    if (d->key[hash] == DICT_EMPTY) {
      continue;
    }

    p_out_loc[i] = d->key[hash] + 1;
    p_out_count[i] = p_count[hash];
    i++;
  }

  out_count = PROTECT_N(r_int_resize(out_count, d->used), &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(VECSXP, 2), &nprot);
  SET_VECTOR_ELT(out, 0, out_loc);
  SET_VECTOR_ELT(out, 1, out_count);
  SEXP names = PROTECT_N(Rf_allocVector(STRSXP, 2), &nprot);
  SET_STRING_ELT(names, 0, Rf_mkChar("loc"));
  SET_STRING_ELT(names, 1, Rf_mkChar("count"));
  Rf_setAttrib(out, R_NamesSymbol, names);
  init_data_frame(out, d->used);

  UNPROTECT(nprot);
  return out;
}

#define VCTRS_COUNT_LOOP(DICT_HASH_SCALAR)      \
do {                                            \
  for (int i = 0; i < n; ++i) {                 \
    uint32_t hash = DICT_HASH_SCALAR(d, i);     \
                                                \
    if (d->key[hash] == DICT_EMPTY) {           \
      dict_put(d, hash, i);                     \
      p_count[hash] = 0;                        \
    }                                           \
                                                \
    p_count[hash]++;                            \
  }                                             \
}                                               \
while (0)

static inline
void vctrs_count_loop(struct dictionary* d, R_len_t n, int* p_count) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VCTRS_COUNT_LOOP(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: VCTRS_COUNT_LOOP(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: VCTRS_COUNT_LOOP(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: VCTRS_COUNT_LOOP(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: VCTRS_COUNT_LOOP(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: VCTRS_COUNT_LOOP(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: VCTRS_COUNT_LOOP(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: VCTRS_COUNT_LOOP(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: VCTRS_COUNT_LOOP(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("vctrs_count_loop", d->p_poly_vec->type);
  }
}

#undef VCTRS_COUNT_LOOP

SEXP vctrs_duplicated(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n), &nprot);
  int* p_out = LOGICAL(out);
  r_memset(p_out, 0, n * sizeof(int));

  uint32_t* p_hashes = (uint32_t*) R_alloc(n, sizeof(uint32_t));

  vctrs_duplicated_loop(d, n, p_hashes, p_out);

  UNPROTECT(nprot);
  return out;
}

#define VCTRS_DUPLICATED_LOOP(DICT_HASH_SCALAR)   \
do {                                              \
  /* Forward pass */                              \
  for (R_len_t i = 0; i < n; ++i) {               \
    const uint32_t hash = DICT_HASH_SCALAR(d, i); \
    p_hashes[i] = hash;                           \
                                                  \
    if (d->key[hash] == DICT_EMPTY) {             \
      dict_put(d, hash, i);                       \
    } else {                                      \
      p_out[i] = 1;                               \
    }                                             \
  }                                               \
                                                  \
  for (uint32_t i = 0; i < d->size; ++i) {        \
    d->key[i] = DICT_EMPTY;                       \
  }                                               \
                                                  \
  /* Reverse pass */                              \
  for (R_len_t i = n - 1; i >= 0; --i) {          \
    const uint32_t hash = p_hashes[i];            \
                                                  \
    if (d->key[hash] == DICT_EMPTY) {             \
      dict_put(d, hash, i);                       \
    } else {                                      \
      p_out[i] = 1;                               \
    }                                             \
  }                                               \
}                                                 \
while (0)

static inline
void vctrs_duplicated_loop(struct dictionary* d, R_len_t n, uint32_t* p_hashes, int* p_out) {
  switch (d->p_poly_vec->type) {
  case VCTRS_TYPE_null: VCTRS_DUPLICATED_LOOP(nil_dict_hash_scalar); break;
  case VCTRS_TYPE_logical: VCTRS_DUPLICATED_LOOP(lgl_dict_hash_scalar); break;
  case VCTRS_TYPE_integer: VCTRS_DUPLICATED_LOOP(int_dict_hash_scalar); break;
  case VCTRS_TYPE_double: VCTRS_DUPLICATED_LOOP(dbl_dict_hash_scalar); break;
  case VCTRS_TYPE_complex: VCTRS_DUPLICATED_LOOP(cpl_dict_hash_scalar); break;
  case VCTRS_TYPE_character: VCTRS_DUPLICATED_LOOP(chr_dict_hash_scalar); break;
  case VCTRS_TYPE_raw: VCTRS_DUPLICATED_LOOP(raw_dict_hash_scalar); break;
  case VCTRS_TYPE_list: VCTRS_DUPLICATED_LOOP(list_dict_hash_scalar); break;
  case VCTRS_TYPE_dataframe: VCTRS_DUPLICATED_LOOP(df_dict_hash_scalar); break;
  default: stop_unimplemented_vctrs_type("vctrs_duplicated_loop", d->p_poly_vec->type);
  }
}

#undef VCTRS_DUPLICATED_LOOP

void vctrs_init_dictionary(SEXP ns) {
  args_needles = new_wrapper_arg(NULL, "needles");
  args_haystack = new_wrapper_arg(NULL, "haystack");
}
