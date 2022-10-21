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

  d->p_equal_na_equal = poly_p_equal_na_equal(type);
  d->p_is_incomplete = poly_p_is_incomplete(type);

  d->used = 0;

  if (opts->partial) {
    d->key = NULL;
    d->size = 0;
  } else {
    uint32_t size = dict_key_size(x);

    d->key = (R_len_t*) R_alloc(size, sizeof(R_len_t));
    memset(d->key, DICT_EMPTY, size * sizeof(R_len_t));

    d->size = size;
  }

  R_len_t n = vec_size(x);
  if (n) {
    d->hash = (uint32_t*) R_alloc(n, sizeof(uint32_t));

    if (!(d->hash)) {
      Rf_errorcall(R_NilValue, "Can't allocate hash lookup table. Please free memory.");
    }

    memset(d->hash, 0, n * sizeof(R_len_t));
    hash_fill(d->hash, n, x, opts->na_equal);
  } else {
    d->hash = NULL;
  }

  UNPROTECT(nprot);
  return d;
}


// Use hash from `x` but value from `d`. `x` does not need a full
// initialisation of the key vector and can be created with
// `new_dictionary_partial()`.
uint32_t dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  uint32_t hash = x->hash[i];

  const void* p_d_vec = d->p_poly_vec->p_vec;
  const void* p_x_vec = x->p_poly_vec->p_vec;

  // Quadratic probing: will try every slot if d->size is power of 2
  // http://research.cs.vt.edu/AVresearch/hashing/quadratic.php
  for (uint32_t k = 0; k < d->size; ++k) {
    uint32_t probe = (hash + k * (k + 1) / 2) & (d->size - 1);
    // Rprintf("Probe: %i\n", probe);

    // If we circled back to start, dictionary is full
    if (k > 1 && probe == hash) {
      break;
    }

    // Check for unused slot
    R_len_t idx = d->key[probe];
    if (idx == DICT_EMPTY) {
      return probe;
    }

    // Check for same value as there might be a collision
    if (d->p_equal_na_equal(p_d_vec, idx, p_x_vec, i)) {
      return probe;
    }

    // Collision. next iteration will find another spot using
    // quadratic probing.
  }

  r_stop_internal("Dictionary is full.");
}

uint32_t dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return dict_hash_with(d, d, i);
}

bool dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return d->hash[i] == HASH_MISSING &&
    d->p_is_incomplete(d->p_poly_vec->p_vec, i);
}


void dict_put(struct dictionary* d, uint32_t hash, R_len_t i) {
  d->key[hash] = i;
  d->used++;
}

// Assume worst case, that every value is distinct, aiming for a load factor
// of at most 77%. We round up to power of 2 to ensure quadratic probing
// strategy works. Maximum power of 2 we can store in a uint32_t is 2^31,
// as 2^32 is 1 greater than the max uint32_t value, so we clamp sizes that
// would result in 2^32 to INT32_MAX to ensure that our maximum ceiling value
// is only 2^31. This will increase the load factor above 77% for `x` with
// length greater than 1653562409 (2147483648 * .77), but it ensures that
// it can run.
static inline
uint32_t dict_key_size(SEXP x) {
  const R_len_t x_size = vec_size(x);

  if (x_size > R_LEN_T_MAX) {
    // Ensure we catch the switch to supporting long vectors in `vec_size()`
    r_stop_internal("Dictionary functions do not support long vectors.");
  }

  const double load_adjusted_size = x_size / 0.77;

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

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      growable_push_int(&g, i + 1);
    }
  }

  SEXP out = growable_values(&g);

  UNPROTECT(nprot);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_unique(SEXP x) {
  SEXP index = PROTECT(vctrs_unique_loc(x));
  SEXP out = vec_slice(x, index);
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

  bool out = false;

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    } else {
      out = true;
      break;
    }
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_n_distinct(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  UNPROTECT(nprot);
  return Rf_ScalarInteger(d->used);
}

SEXP vctrs_id(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out = INTEGER(out);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
    p_out[i] = d->key[hash] + 1;
  }

  UNPROTECT(nprot);
  return out;
}

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

static inline void vec_match_loop(int* p_out,
                                  struct dictionary* d,
                                  struct dictionary* d_needles,
                                  R_len_t n_needle);
static inline void vec_match_loop_propagate(int* p_out,
                                            struct dictionary* d,
                                            struct dictionary* d_needles,
                                            R_len_t n_needle);

SEXP vec_match_params(SEXP needles,
                      SEXP haystack,
                      bool na_equal,
                      struct vctrs_arg* needles_arg,
                      struct vctrs_arg* haystack_arg,
                      struct r_lazy call) {
  int nprot = 0;
  int _;
  SEXP type = vec_ptype2_params(needles, haystack,
                                needles_arg, haystack_arg,
                                call,
                                &_);
  PROTECT_N(type, &nprot);

  needles = vec_cast_params(needles, type,
                            needles_arg, vec_args.empty,
                            call,
                            S3_FALLBACK_false);
  PROTECT_N(needles, &nprot);

  haystack = vec_cast_params(haystack, type,
                             haystack_arg, vec_args.empty,
                             call,
                             S3_FALLBACK_false);
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
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

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

static inline void vec_match_loop(int* p_out,
                                  struct dictionary* d,
                                  struct dictionary* d_needles,
                                  R_len_t n_needle) {
  for (R_len_t i = 0; i < n_needle; ++i) {
    uint32_t hash = dict_hash_with(d, d_needles, i);

    if (d->key[hash] == DICT_EMPTY) {
      // TODO: Return `no_match` instead
      p_out[i] = NA_INTEGER;
    } else {
      p_out[i] = d->key[hash] + 1;
    }
  }
}
static inline void vec_match_loop_propagate(int* p_out,
                                            struct dictionary* d,
                                            struct dictionary* d_needles,
                                            R_len_t n_needle) {
  for (R_len_t i = 0; i < n_needle; ++i) {
    if (dict_is_incomplete(d_needles, i)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    uint32_t hash = dict_hash_with(d, d_needles, i);

    if (d->key[hash] == DICT_EMPTY) {
      // TODO: Return `no_match` instead
      p_out[i] = NA_INTEGER;
    } else {
      p_out[i] = d->key[hash] + 1;
    }
  }
}

// [[ register() ]]
SEXP vctrs_in(SEXP needles, SEXP haystack, SEXP na_equal_, SEXP frame) {
  struct r_lazy call = { .x = frame, .env = r_null };

  int nprot = 0;
  bool na_equal = r_bool_as_int(na_equal_);

  int _;

  struct r_lazy needles_arg_ = { .x = syms.needles_arg, .env = frame };
  struct vctrs_arg needles_arg = new_lazy_arg(&needles_arg_);

  struct r_lazy haystack_arg_ = { .x = syms.haystack_arg, .env = frame };
  struct vctrs_arg haystack_arg = new_lazy_arg(&haystack_arg_);

  SEXP type = vec_ptype2_params(needles, haystack,
                                &needles_arg, &haystack_arg,
                                call,
                                &_);
  PROTECT_N(type, &nprot);

  needles = vec_cast_params(needles, type,
                            &needles_arg, vec_args.empty,
                            call,
                            S3_FALLBACK_false);
  PROTECT_N(needles, &nprot);

  haystack = vec_cast_params(haystack, type,
                             &haystack_arg, vec_args.empty,
                             call,
                             S3_FALLBACK_false);
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
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  struct dictionary* d_needles = new_dictionary_params(needles, true, na_equal);
  PROTECT_DICT(d_needles, &nprot);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n_needle), &nprot);
  int* p_out = LOGICAL(out);

  bool propagate = !na_equal;

  for (int i = 0; i < n_needle; ++i) {
    if (propagate && dict_is_incomplete(d_needles, i)) {
      p_out[i] = NA_LOGICAL;
    } else {
      uint32_t hash = dict_hash_with(d, d_needles, i);
      p_out[i] = (d->key[hash] != DICT_EMPTY);
    }
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_count(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP count = PROTECT_N(Rf_allocVector(INTSXP, d->size), &nprot);
  int* p_count = INTEGER(count);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_count[hash] = 0;
    }
    p_count[hash]++;
  }

  // Create output
  SEXP out_loc = PROTECT_N(Rf_allocVector(INTSXP, d->used), &nprot);
  int* p_out_loc = INTEGER(out_loc);

  // Reuse `count` storage, which will be narrowed
  SEXP out_count = count;
  int* p_out_count = p_count;

  int i = 0;
  for (uint32_t hash = 0; hash < d->size; ++hash) {
    if (d->key[hash] == DICT_EMPTY)
      continue;

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

SEXP vctrs_duplicated(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(vec_normalize_encoding(x), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n), &nprot);
  int* p_out = LOGICAL(out);
  memset(p_out, 0, n * sizeof(int));

  uint32_t* p_hashes = (uint32_t*) R_alloc(n, sizeof(uint32_t));

  // Forward pass
  for (R_len_t i = 0; i < n; ++i) {
    const uint32_t hash = dict_hash_scalar(d, i);
    p_hashes[i] = hash;

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    } else {
      p_out[i] = 1;
    }
  }

  for (uint32_t i = 0; i < d->size; ++i) {
    d->key[i] = DICT_EMPTY;
  }

  // Reverse pass
  for (R_len_t i = n - 1; i >= 0; --i) {
    const uint32_t hash = p_hashes[i];

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    } else {
      p_out[i] = 1;
    }
  }

  UNPROTECT(nprot);
  return out;
}


void vctrs_init_dictionary(SEXP ns) {
  args_needles = new_wrapper_arg(NULL, "needles");
  args_haystack = new_wrapper_arg(NULL, "haystack");
}
