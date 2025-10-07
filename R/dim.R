#' Actual vector dimensions
#'
#' @description
#' * `vec_dim_n()` gives the dimensionality (i.e. number of dimensions)
#' * `vec_dim()` returns the size of each dimension
#'
#' These functions access the raw `"dim"` attribute of the object
#' and do not dispatch over the [dim()] generic.
#'
#' @details
#' Unlike base R, we treat vectors with `NULL` dimensions as 1d. This
#' simplifies the type system by eliding a special case. Compared to the base R
#' equivalent, `vec_dim()` returns [length()], not `NULL`, when `x` is 1d.
#'
#' @seealso
#' `dim2()`, a variant of [dim()] that returns [length()] if an object
#' doesn't have dimensions.
#'
#' @param x A vector
#' @noRd
#' @examples
#' # Compared to base R
#' x <- 1:5
#' dim(x)
#' vec_dim(x)
NULL

# FIXME: Should `vec_dim()` return the size instead of the length?
vec_dim <- function(x) {
  .Call(vctrs_dim, x)
}
vec_dim_n <- function(x) {
  .Call(vctrs_dim_n, x)
}
has_dim <- function(x) {
  .Call(vctrs_has_dim, x)
}

#' Perceived vector dimensions
#'
#' @description
#' `dim2()` is a variant of [dim()] that returns [vec_size()] if an object
#' doesn't have dimensions.
#'
#' @details
#' Unlike base R, we treat vectors with `NULL` dimensions as 1d. This
#' simplifies the type system by eliding a special case. Compared to the base R
#' equivalent, `dim2()` returns [length()], not `NULL`, when `x` is 1d.
#'
#' @seealso
#' `vec_dim()`, a variant that never dispatches over the [dim()] generic.
#'
#' @param x A vector
#' @noRd
#' @examples
#' # Compared to base R
#' x <- 1:5
#' dim(x)
#' vec_dim(x)
dim2 <- function(x) {
  dim(x) %||% length(x)
}
