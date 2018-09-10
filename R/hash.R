hash <- function(x) {
  as.hexmode(.Call(vctrs_hash, x))
}

hash_obj <- function(x) {
  as.hexmode(.Call(vctrs_hash_object, x))
}

hash_label <- function(x, length = 5) {
  if (length(x) == 0) {
    ""
  } else {
    # Can't use hash() currently because it hashes the string pointers
    # for performance, so the values in the test change each time
    substr(digest::digest(x), 1, length)
  }
}
