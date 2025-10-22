#include "hash.h"
#include "vctrs.h"

#include "decl/hash-decl.h"

// ----------------------------------------------------------------------------
// Object

r_obj* ffi_obj_hash(r_obj* x) {
  uint32_t hash = 0;
  hash = hash_combine(hash, obj_hash(x));

  r_obj* out = KEEP(r_alloc_raw(sizeof(uint32_t)));
  r_memcpy(r_raw_begin(out), &hash, sizeof(uint32_t));
  FREE(1);
  return out;
}

uint32_t obj_hash(r_obj* x) {
  uint32_t hash = sexp_hash(x);

  r_obj* attrib = r_attrib(x);
  if (attrib != r_null) {
    hash = hash_combine(hash, obj_hash(attrib));
  }

  return hash;
}

static inline uint32_t sexp_hash(r_obj* x) {
  switch (TYPEOF(x)) {
  // `NULL`
  case NILSXP: return 0;

  // Atomics
  case LGLSXP: return lgl_hash(x);
  case INTSXP: return int_hash(x);
  case REALSXP: return dbl_hash(x);
  case CPLXSXP: return cpl_hash(x);
  case RAWSXP: return raw_hash(x);
  case STRSXP: return chr_hash(x);
  case VECSXP: return list_hash(x);

  // Expressions
  case EXPRSXP: return expr_hash(x);

  // Node-like
  case DOTSXP:
  case LANGSXP:
  case LISTSXP:
  case BCODESXP: return node_hash(x);

  // Functions
  case CLOSXP: return fn_hash(x);

  // Pointer based hashing
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case ENVSXP:
  case EXTPTRSXP: return uint64_hash((uintptr_t) x);

  default: Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

#define HASH(CTYPE, CONST_DEREF, HASHER)        \
  uint32_t hash = 0;                            \
  const r_ssize size = r_length(x);             \
  CTYPE const* v_x = CONST_DEREF(x);            \
                                                \
  for (r_ssize i = 0; i < size; ++i) {          \
    hash = hash_combine(hash, HASHER(v_x[i]));  \
  }                                             \
                                                \
  return hash

static inline uint32_t lgl_hash(r_obj* x) {
  HASH(int, r_lgl_cbegin, lgl_hash_scalar);
}
static inline uint32_t int_hash(r_obj* x) {
  HASH(int, r_int_cbegin, int_hash_scalar);
}
static inline uint32_t dbl_hash(r_obj* x) {
  HASH(double, r_dbl_cbegin, dbl_hash_scalar);
}
static inline uint32_t cpl_hash(r_obj* x) {
  HASH(r_complex, r_cpl_cbegin, cpl_hash_scalar);
}
static inline uint32_t raw_hash(r_obj* x) {
  HASH(Rbyte, r_raw_cbegin, raw_hash_scalar);
}
static inline uint32_t chr_hash(r_obj* x) {
  HASH(r_obj*, r_chr_cbegin, chr_hash_scalar);
}
static inline uint32_t list_hash(r_obj* x) {
  HASH(r_obj*, r_list_cbegin, list_hash_scalar);
}

#undef HASH

static inline uint32_t expr_hash(r_obj* x) {
  uint32_t hash = 0;
  r_ssize n = Rf_xlength(x);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = r_list_get(x, i);
    hash = hash_combine(hash, obj_hash(elt));
  }

  return hash;
}

static inline uint32_t node_hash(r_obj* x) {
  uint32_t hash = 0;
  hash = hash_combine(hash, obj_hash(r_node_car(x)));
  hash = hash_combine(hash, obj_hash(r_node_cdr(x)));
  return hash;
}

static inline uint32_t fn_hash(r_obj* x) {
  uint32_t hash = 0;
  hash = hash_combine(hash, obj_hash(r_fn_body(x)));
  hash = hash_combine(hash, obj_hash(r_fn_env(x)));
  hash = hash_combine(hash, obj_hash(FORMALS(x)));
  return hash;
}

// ----------------------------------------------------------------------------
// Vector hash

r_obj* ffi_vec_hash(r_obj* x) {
  x = KEEP(vec_proxy_equal(x));

  const r_ssize size = vec_size(x);
  r_obj* out = KEEP(r_alloc_raw(size * sizeof(uint32_t)));
  uint32_t* v_out = (uint32_t*) r_raw_begin(out);

  r_memset(v_out, 0, size * sizeof(uint32_t));
  vec_hash_fill(x, size, true, v_out);

  FREE(2);
  return out;
}

// Not compatible with hash_scalar. When `@na_equal` is false, missing
// values are propagated and encoded as `1`.
void vec_hash_fill(r_obj* x, r_ssize size, bool na_equal, uint32_t* v_out) {
  if (has_dim(x)) {
    // The conversion to data frame is only a stopgap, in the long
    // term, we'll hash arrays natively
    x = KEEP(r_as_data_frame(x));
    vec_hash_fill(x, size, na_equal, v_out);
    FREE(1);
    return;
  }

  if (na_equal) {
    switch (vec_proxy_typeof(x)) {
    case VCTRS_TYPE_logical: lgl_hash_fill_na_equal(x, size, v_out); return;
    case VCTRS_TYPE_integer: int_hash_fill_na_equal(x, size, v_out); return;
    case VCTRS_TYPE_double: dbl_hash_fill_na_equal(x, size, v_out); return;
    case VCTRS_TYPE_complex: cpl_hash_fill_na_equal(x, size, v_out); return;
    case VCTRS_TYPE_character: chr_hash_fill_na_equal(x, size, v_out); return;
    case VCTRS_TYPE_raw: raw_hash_fill_na_equal(x, size, v_out); return;
    case VCTRS_TYPE_list: list_hash_fill_na_equal(x, size, v_out); return;
    case VCTRS_TYPE_dataframe: df_hash_fill(x, size, na_equal, v_out); return;
    default: break;
    }
  } else {
    switch (vec_proxy_typeof(x)) {
    case VCTRS_TYPE_logical: lgl_hash_fill_na_propagate(x, size, v_out); return;
    case VCTRS_TYPE_integer: int_hash_fill_na_propagate(x, size, v_out); return;
    case VCTRS_TYPE_double: dbl_hash_fill_na_propagate(x, size, v_out); return;
    case VCTRS_TYPE_complex: cpl_hash_fill_na_propagate(x, size, v_out); return;
    case VCTRS_TYPE_character: chr_hash_fill_na_propagate(x, size, v_out); return;
    case VCTRS_TYPE_raw: raw_hash_fill_na_propagate(x, size, v_out); return;
    case VCTRS_TYPE_list: list_hash_fill_na_propagate(x, size, v_out); return;
    case VCTRS_TYPE_dataframe: df_hash_fill(x, size, na_equal, v_out); return;
    default: break;
    }
  }

  stop_unimplemented_vctrs_type("vec_hash_fill", vec_proxy_typeof(x));
}

#define HASH_FILL(CTYPE, CONST_DEREF, HASHER)           \
  CTYPE const* v_x = CONST_DEREF(x);                    \
                                                        \
  for (r_ssize i = 0; i < size; ++i) {                  \
    const uint32_t hash = v_out[i];                     \
    CTYPE elt = v_x[i];                                 \
    v_out[i] = hash_combine(hash, HASHER(elt));         \
  }

// Incomplete rows in data frames propagate an `NA` hash
#define HASH_FILL_NA_PROPAGATE(CTYPE, CONST_DEREF, HASHER, IS_MISSING)           \
  CTYPE const* v_x = CONST_DEREF(x);                                             \
                                                                                 \
  for (r_ssize i = 0; i < size; ++i) {                                           \
    const uint32_t hash = v_out[i];                                              \
    if (hash == HASH_MISSING) {                                                  \
      continue;                                                                  \
    }                                                                            \
    CTYPE elt = v_x[i];                                                          \
    v_out[i] = IS_MISSING(elt) ? HASH_MISSING : hash_combine(hash, HASHER(elt)); \
  }

static inline void lgl_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL(int, r_lgl_cbegin, lgl_hash_scalar);
}
static inline void int_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL(int, r_int_cbegin, int_hash_scalar);
}
static inline void dbl_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL(double, r_dbl_cbegin, dbl_hash_scalar);
}
static inline void cpl_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL(r_complex, r_cpl_cbegin, cpl_hash_scalar);
}
static inline void chr_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL(r_obj*, r_chr_cbegin, chr_hash_scalar);
}
static inline void raw_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL(Rbyte, r_raw_cbegin, raw_hash_scalar);
}
static inline void list_hash_fill_na_equal(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL(r_obj*, r_list_cbegin, list_hash_scalar);
}

static inline void lgl_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL_NA_PROPAGATE(int, r_lgl_cbegin, lgl_hash_scalar, lgl_is_missing);
}
static inline void int_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL_NA_PROPAGATE(int, r_int_cbegin, int_hash_scalar, int_is_missing);
}
static inline void dbl_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL_NA_PROPAGATE(double, r_dbl_cbegin, dbl_hash_scalar, dbl_is_missing);
}
static inline void cpl_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL_NA_PROPAGATE(r_complex, r_cpl_cbegin, cpl_hash_scalar, cpl_is_missing);
}
static inline void chr_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL_NA_PROPAGATE(r_obj*, r_chr_cbegin, chr_hash_scalar, chr_is_missing);
}
static inline void raw_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL_NA_PROPAGATE(Rbyte, r_raw_cbegin, raw_hash_scalar, raw_is_missing);
}
static inline void list_hash_fill_na_propagate(r_obj* x, r_ssize size, uint32_t* v_out) {
  HASH_FILL_NA_PROPAGATE(r_obj*, r_list_cbegin, list_hash_scalar, list_is_missing);
}

#undef HASH_FILL_NA_PROPAGATE
#undef HASH_FILL

static inline void df_hash_fill(r_obj* x, r_ssize size, bool na_equal, uint32_t* v_out) {
  const r_ssize n_col = r_length(x);
  r_obj* const* v_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < n_col; ++i) {
    vec_hash_fill(v_x[i], size, na_equal, v_out);
  }
}
