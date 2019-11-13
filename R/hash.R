
# These return raw vectors of hashes. Vector elements are coded with
# 32 bit hashes. Thus, the size of the raw vector of hashes is 4 times
# the size of the input.

vec_hash <- function(x, pool = TRUE) {
  vec_assert(pool, ptype = logical(), size = 1L)
  .Call(vctrs_hash, x, pool)
}

obj_hash <- function(x, pool = TRUE) {
  vec_assert(pool, ptype = logical(), size = 1L)
  .Call(vctrs_hash_object, x, pool)
}
