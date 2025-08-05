#ifndef VCTRS_DICTIONARY_H
#define VCTRS_DICTIONARY_H

#include "vctrs-core.h"
#include "poly-op.h"
#include "equal.h"
#include "hash.h"

#define DICT_EMPTY -1

// The dictionary structure is a little peculiar since R has no notion of
// a scalar, so the `key`s are indexes into vector `x`. This means we can
// only store values from a single vector, but we can still lookup using
// another vector, provided that they're of the same type (which is ensured
// at the R-level).

struct dictionary {
  SEXP protect;

  struct poly_vec* p_poly_vec;

  uint32_t* hash;
  R_len_t* key;

  uint32_t size;
  uint32_t used;
};

/**
 * Initialise a dictionary
 *
 * - `new_dictionary()` creates a dictionary and precaches the hashes for
 *   each element of `x`.
 *
 * - `new_dictionary_partial()` creates a dictionary with precached hashes
 *   as well, but does not allocate an array of keys. This is useful
 *   for finding a key in another dictionary with `*_dict_hash_with()`.
 */

struct dictionary_opts {
  bool partial;
  bool na_equal;
};

struct dictionary* new_dictionary(SEXP x);
struct dictionary* new_dictionary_partial(SEXP x);

#define PROTECT_DICT(d, n) do {        \
  struct dictionary* d_ = (d);         \
  KEEP(d_->p_poly_vec->shelter);       \
  KEEP(d_->protect);                   \
  *(n) += 2;                           \
} while(0)

static inline
void dict_put(struct dictionary* d, uint32_t hash, R_len_t i) {
  d->key[hash] = i;
  d->used++;
}

// This is an EXTREMELY hot loop. The `*_dict_hash_with()` functions
// themselves are always called within an outer loop over a vector,
// so it is almost always a double loop. For performance, this makes
// it very important to ensure that the `*_dict_hash_with()` function
// is inlined (we have seen good evidence that this helps a lot, up
// to a factor of 2x in some cases).
//
// To encourage inlining, we make typed wrappers over `P_EQUAL_NA_EQUAL`
// which are marked with `static inline` and then we use these in
// the dictionary functions.
//
// Quadratic probing: will try every slot if d->size is power of 2
// http://research.cs.vt.edu/AVresearch/hashing/quadratic.php
#define DICT_HASH_WITH(P_EQUAL_NA_EQUAL)                         \
do {                                                             \
  uint32_t hash = x->hash[i];                                    \
                                                                 \
  const void* p_d_vec = d->p_poly_vec->p_vec;                    \
  const void* p_x_vec = x->p_poly_vec->p_vec;                    \
  const uint32_t size = d->size;                                 \
                                                                 \
  for (uint32_t k = 0; k < size; ++k) {                          \
    uint32_t probe = (hash + k * (k + 1) / 2) & (d->size - 1);   \
    /* Rprintf("Probe: %i\n", probe); */                         \
                                                                 \
    /* If we circled back to start, dictionary is full */        \
    if (k > 1 && probe == hash) {                                \
      break;                                                     \
    }                                                            \
                                                                 \
    /* Check for unused slot */                                  \
    R_len_t idx = d->key[probe];                                 \
    if (idx == DICT_EMPTY) {                                     \
      return probe;                                              \
    }                                                            \
                                                                 \
    /* Check for same value as there might be a collision */     \
    if (P_EQUAL_NA_EQUAL(p_d_vec, idx, p_x_vec, i)) {            \
      return probe;                                              \
    }                                                            \
                                                                 \
    /* Collision. next iteration will find another spot using */ \
    /* quadratic probing. */                                     \
  }                                                              \
  r_stop_internal("Dictionary is full.");                        \
}                                                                \
while (0)

/**
 * `*_dict_hash_with()` finds the hash for indexing into `d` with
 * element `i` of `x`.
 */
static inline
uint32_t nil_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_nil_equal_na_equal);
}
static inline
uint32_t lgl_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_lgl_equal_na_equal);
}
static inline
uint32_t int_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_int_equal_na_equal);
}
static inline
uint32_t dbl_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_dbl_equal_na_equal);
}
static inline
uint32_t cpl_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_cpl_equal_na_equal);
}
static inline
uint32_t chr_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_chr_equal_na_equal);
}
static inline
uint32_t raw_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_raw_equal_na_equal);
}
static inline
uint32_t list_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_list_equal_na_equal);
}
static inline
uint32_t df_dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  DICT_HASH_WITH(p_df_equal_na_equal);
}

#undef DICT_HASH_WITH


/**
 * `*_dict_hash_scalar()` returns the key hash for element `i`.
 */
static inline
uint32_t nil_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return nil_dict_hash_with(d, d, i);
}
static inline
uint32_t lgl_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return lgl_dict_hash_with(d, d, i);
}
static inline
uint32_t int_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return int_dict_hash_with(d, d, i);
}
static inline
uint32_t dbl_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return dbl_dict_hash_with(d, d, i);
}
static inline
uint32_t cpl_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return cpl_dict_hash_with(d, d, i);
}
static inline
uint32_t chr_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return chr_dict_hash_with(d, d, i);
}
static inline
uint32_t raw_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return raw_dict_hash_with(d, d, i);
}
static inline
uint32_t list_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return list_dict_hash_with(d, d, i);
}
static inline
uint32_t df_dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return df_dict_hash_with(d, d, i);
}


static inline
bool dict_hash_is_missing(struct dictionary* d, R_len_t i) {
  return d->hash[i] == HASH_MISSING;
}

static inline
bool nil_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_nil_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool lgl_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_lgl_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool int_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_int_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool dbl_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_dbl_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool cpl_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_cpl_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool chr_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_chr_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool raw_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_raw_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool list_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_list_is_incomplete(d->p_poly_vec->p_vec, i);
}
static inline
bool df_dict_is_incomplete(struct dictionary* d, R_len_t i) {
  return dict_hash_is_missing(d, i) && p_df_is_incomplete(d->p_poly_vec->p_vec, i);
}

#endif
