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
#' @param x A vector
#' @name dim
#' @examples
#' # Compared to base R
#' x <- 1:5
#' dim(x)
#' vec_dim(x)
NULL

#' @export
#' @rdname dim
vec_dim <- function(x) {
  .Call(vctrs_dim, x)
}

#' @export
#' @rdname dim
vec_dim_n <- function(x) {
  .Call(vctrs_dim_n, x)
}

# Unexported counterpart to `vec_dim()` that respects that `dim()` is a generic.
# Data frames looks like 2d structures with this.
dim2 <- function(x) {
  dim(x) %||% length(x)
}
