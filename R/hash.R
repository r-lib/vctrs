vec_hash <- function(x) {
  x <- vec_proxy_equal(x)
  as.hexmode(.Call(vctrs_hash, x))
}

obj_hash <- function(x) {
  as.hexmode(.Call(vctrs_hash_object, x))
}
