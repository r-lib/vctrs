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

vec_equal <- function(x, y, .ptype = NULL) {
  args <- vec_recycle(x, y)
  args <- vec_coerce(!!!args, .ptype = .ptype)
  .Call(vctrs_equal, vec_proxy_equality(args[[1]]), vec_proxy_equality(args[[2]]))
}
