
#define DICT_EMPTY -1


// The dictionary structure is a little peculiar since R has no notion of
// a scalar, so the `key`s are indexes into vector `x`. This means we can
// only store values from a single vector, but we can still lookup using
// another vector, provided that they're of the same type (which is ensured
// at the R-level).

struct dictionary {
  SEXP vec;
  R_len_t* key;
  uint32_t* hash;
  uint32_t size;
  uint32_t used;
};
typedef struct dictionary dictionary;

/**
 * Initialise a dictionary
 *
 * - `dict_init()` creates a dictionary and precaches the hashes for
 *   each element of `x`.
 *
 * - `dict_init_partial()` creates a dictionary with precached hashes
 *   as well, but does not allocate an array of keys. This is useful
 *   for finding a key in another dictionary with `dict_hash_with()`.
 */
void dict_init(dictionary* d, SEXP x);
void dict_init_partial(dictionary* d, SEXP x);

#define PROTECT_DICT(d, n) do {                 \
    PROTECT((d)->vec);                          \
    *(n) += 1;                                  \
  } while(0)


/**
 * Find key hash for a vector element
 *
 * - `dict_hash_scalar()` returns the key hash for element `i`.
 *
 * - `dict_hash_with()` finds the hash for indexing into `d` with
 *   element `i` of `x`.
 */
uint32_t dict_hash_scalar(dictionary* d, R_len_t i);
uint32_t dict_hash_with(dictionary* d, dictionary* x, R_len_t i);

void dict_put(dictionary* d, uint32_t k, R_len_t i);
