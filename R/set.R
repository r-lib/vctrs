#' Set operations
#'
#' @description
#' - `vec_set_intersect()` returns all values in both `x` and `y`.
#'
#' - `vec_set_difference()` returns all values in `x` but not `y`. Note
#'   that this is an asymmetric set difference, meaning it is not commutative.
#'
#' - `vec_set_union()` returns all values in either `x` or `y`.
#'
#' - `vec_set_symmetric_difference()` returns all values in either `x` or `y`
#'   but not both. This is a commutative difference.
#'
#' Because these are _set_ operations, these functions only return unique values
#' from `x` and `y`, returned in the order they first appeared in the original
#' input. Names of `x` and `y` are retained on the result, but names are always
#' taken from `x` if the value appears in both inputs.
#'
#' These functions work similarly to [base::intersect()], [base::setdiff()], and
#' [base::union()], but don't strip attributes and can be used with data frames.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x,y A pair of vectors.
#'
#' @param ptype If `NULL`, the default, the output type is determined by
#'   computing the common type between `x` and `y`. If supplied, both `x` and
#'   `y` will be cast to this type.
#'
#' @param x_arg,y_arg Argument names for `x` and `y`. These are used in error
#'   messages.
#'
#' @returns
#' A vector of the common type of `x` and `y` (or `ptype`, if supplied)
#' containing the result of the corresponding set function.
#'
#' @details
#' Missing values are treated as equal to other missing values. For doubles and
#' complexes, `NaN` are equal to other `NaN`, but not to `NA`.
#'
#' @section Dependencies:
#'
#' ## `vec_set_intersect()`
#' - [vec_proxy_equal()]
#' - [vec_slice()]
#' - [vec_ptype2()]
#' - [vec_cast()]
#'
#' ## `vec_set_difference()`
#' - [vec_proxy_equal()]
#' - [vec_slice()]
#' - [vec_ptype2()]
#' - [vec_cast()]
#'
#' ## `vec_set_union()`
#' - [vec_proxy_equal()]
#' - [vec_slice()]
#' - [vec_ptype2()]
#' - [vec_cast()]
#' - [vec_c()]
#'
#' ## `vec_set_symmetric_difference()`
#' - [vec_proxy_equal()]
#' - [vec_slice()]
#' - [vec_ptype2()]
#' - [vec_cast()]
#' - [vec_c()]
#'
#' @name vec-set
#' @examples
#' x <- c(1, 2, 1, 4, 3)
#' y <- c(2, 5, 5, 1)
#'
#' # All unique values in both `x` and `y`.
#' # Duplicates in `x` and `y` are always removed.
#' vec_set_intersect(x, y)
#'
#' # All unique values in `x` but not `y`
#' vec_set_difference(x, y)
#'
#' # All unique values in either `x` or `y`
#' vec_set_union(x, y)
#'
#' # All unique values in either `x` or `y` but not both
#' vec_set_symmetric_difference(x, y)
#'
#' # These functions can also be used with data frames
#' x <- data_frame(
#'   a = c(2, 3, 2, 2),
#'   b = c("j", "k", "j", "l")
#' )
#' y <- data_frame(
#'   a = c(1, 2, 2, 2, 3),
#'   b = c("j", "l", "j", "l", "j")
#' )
#'
#' vec_set_intersect(x, y)
#' vec_set_difference(x, y)
#' vec_set_union(x, y)
#' vec_set_symmetric_difference(x, y)
#'
#' # Vector names don't affect set membership, but if you'd like to force
#' # them to, you can transform the vector into a two column data frame
#' x <- c(a = 1, b = 2, c = 2, d = 3)
#' y <- c(c = 2, b = 1, a = 3, d = 3)
#'
#' vec_set_intersect(x, y)
#'
#' x <- data_frame(name = names(x), value = unname(x))
#' y <- data_frame(name = names(y), value = unname(y))
#'
#' vec_set_intersect(x, y)
NULL

#' @rdname vec-set
#' @export
vec_set_intersect <- function(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(ffi_vec_set_intersect, x, y, ptype, environment())
}

#' @rdname vec-set
#' @export
vec_set_difference <- function(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(ffi_vec_set_difference, x, y, ptype, environment())
}

#' @rdname vec-set
#' @export
vec_set_union <- function(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(ffi_vec_set_union, x, y, ptype, environment())
}

#' @rdname vec-set
#' @export
vec_set_symmetric_difference <- function(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(ffi_vec_set_symmetric_difference, x, y, ptype, environment())
}
