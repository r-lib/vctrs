#' Hash a vector
#'
#' `vec_hash()` hashes a vector and returns the hash as a raw vector. Each
#' element of `x` is coded with 32-bit hashes. This results in a raw vector
#' that is 4 times the size of `x`.
#'
#' @param x A vector
#' @param pool A logical. Should R's global string pool be used to hash
#'   characters? If `TRUE`, the hashing will be faster, but will not be
#'   reproducible between R sessions.
#' @return A raw vector of hash values of size `4 * vec_size(x)`.
#' @export
#' @examples
#' vec_hash(1:2)
#'
#' vec_hash(mtcars)
#'
#' # To consistently get a size 4 hash for any
#' # vector, wrap it in a list
#' vec_hash(list(mtcars))
vec_hash <- function(x, pool = TRUE) {
  vec_assert(pool, ptype = logical(), size = 1L)
  .Call(vctrs_hash, x, pool)
}

obj_hash <- function(x, pool = TRUE) {
  vec_assert(pool, ptype = logical(), size = 1L)
  .Call(vctrs_hash_object, x, pool)
}
