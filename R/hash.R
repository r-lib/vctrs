vec_hash <- function(x, rowwise = TRUE) {
  as.hexmode(.Call(vctrs_hash, x, rowwise))
}

obj_hash <- function(x) {
  as.hexmode(.Call(vctrs_hash_object, x))
}
