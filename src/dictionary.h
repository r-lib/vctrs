#ifndef VCTRS_DICTIONARY_H
#define VCTRS_DICTIONARY_H

#include "vctrs-core.h"
#include "poly-op.h"

#define DICT_EMPTY -1

// The dictionary structure is a little peculiar since R has no notion of
// a scalar, so the `key`s are indexes into vector `x`. This means we can
// only store values from a single vector, but we can still lookup using
// another vector, provided that they're of the same type (which is ensured
// at the R-level).

struct dictionary {
  SEXP protect;

  poly_binary_int_fn_ptr p_equal_na_equal;
  poly_unary_bool_fn_ptr p_is_incomplete;
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
 *   for finding a key in another dictionary with `dict_hash_with()`.
 */

struct dictionary_opts {
  bool partial;
  bool na_equal;
};

struct dictionary* new_dictionary(SEXP x);
struct dictionary* new_dictionary_partial(SEXP x);

#define PROTECT_DICT(d, n) do {        \
  struct dictionary* d_ = (d);         \
  PROTECT_POLY_VEC(d_->p_poly_vec, n); \
  PROTECT(d_->protect);                \
  *(n) += 1;                           \
} while(0)

/**
 * Find key hash for a vector element
 *
 * - `dict_hash_scalar()` returns the key hash for element `i`.
 *
 * - `dict_hash_with()` finds the hash for indexing into `d` with
 *   element `i` of `x`.
 */
uint32_t dict_hash_scalar(struct dictionary* d, R_len_t i);
uint32_t dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i);

bool dict_is_incomplete(struct dictionary* d, R_len_t i);

void dict_put(struct dictionary* d, uint32_t k, R_len_t i);


#endif
