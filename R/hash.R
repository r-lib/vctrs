#' Hash an object
#'
#' @description
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' `obj_hash()` hashes an object by coding each element of `x` with 32-bit
#' hashes and combining them together. The hash is returned as a raw vector of
#' size 4.
#'
#' @details
#' `obj_hash()` is extremely experimental, and no strong reproducibility
#' assumptions should be made. Because of this, if you use `obj_hash()` the
#' hash results should not be tested in unit tests as they might change without
#' warning.
#'
#' It is possible that some objects have a non-trivial chance to produce the
#' same hash value.
#'
#' @param x A vector
#' @param pool A logical. Should R's global string pool be used to hash
#'   characters? If `TRUE`, the hashing will be faster, but will not be
#'   reproducible between R sessions.
#' @return The hash value as a raw vector of size 4.
#' @export
#' @examples
#' obj_hash(1:2)
#' obj_hash(mtcars)
obj_hash <- function(x, pool = TRUE) {
  vec_assert(pool, ptype = logical(), size = 1L)
  .Call(vctrs_hash_object, x, pool)
}

vec_hash <- function(x, pool = TRUE) {
  vec_assert(pool, ptype = logical(), size = 1L)
  .Call(vctrs_hash, x, pool)
}
