#' Vector dimensions
#'
#' * `vec_obs()` returns the number of observations in an object. This is
#'   the length of a 1d vector, or the number of rows in a matrix or data frame
#'   rows if multidimensional
#' * `vec_empty()` returns `TRUE` if `vec_obs()` is zero.
#' * `vec_dims()` gives the dimensionality (i.e. number of dimensions)
#' * `vec_dim()` returns the size of each dimension
#'
#' Unlike base R, we treat vectors with `NULL` dimensions as 1d. This
#' simplifies the type system by eliding a special case. Compared to base R
#' equivalents `vec_obs()` acts like `NROW()`, and `vec_dim()` returns
#' `length()`, not `NULL`, when `x` is 1d.
#'
#' @param x A vector
#' @name dims
#' @examples
#' # Compared to base R
#' x <- 1:5
#' dim(x)
#' vec_dim(x)
#'
#' y <- matrix(1:6, nrow = 2)
#' length(y)
#' vec_obs(y)
NULL

#' @export
#' @rdname dims
vec_obs <- function(x) {
  .Call(vctrs_length, x)
}

#' @export
#' @rdname dims
vec_empty <- function(x) {
  vec_obs(x) == 0L
}

#' @export
#' @rdname dims
vec_dim <- function(x) {
  dim(x) %||% length(x)
}

#' @export
#' @rdname dims
vec_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}
