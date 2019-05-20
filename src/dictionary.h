
#define DICT_EMPTY -1


// The dictionary structure is a little peculiar since R has no notion of
// a scalar, so the `key`s are indexes into vector `x`. This means we can
// only store values from a single vector, but we can still lookup using
// another vector, provided that they're of the same type (which is ensured
// at the R-level).

struct dictionary {
  SEXP x;
  R_len_t* key;
  uint32_t* hash;
  uint32_t size;
  uint32_t used;
};
typedef struct dictionary dictionary;

void dict_init(dictionary* d, SEXP x, bool hashed);
void dict_free(dictionary* d);
uint32_t dict_hash_scalar(dictionary* d, SEXP y, R_len_t i);
void dict_put(dictionary* d, uint32_t k, R_len_t i);
