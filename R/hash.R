hash <- function(x) {
  .Call(vctrs_hash, x)
}

hash_vector <- function(x) {
  .Call(vctrs_hash_vector, x)
}
