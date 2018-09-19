#' Numeric proxy
#'
#' `vec_proxy_numeric()` converts an object to it's numeric representation
#' (if available). `vec_restore_numeric()` restores a numeric proxy back to
#' the original type. The default methods allow any object built on a logical,
#' integer, or numeric, to be coerced to numeric, and leave the result as is.
#'
#' @keywords internal
#' @export
#' @examples
#' x <- new_vctr(-10:10)
#'
#' abs(x)
#' x ^ 2
vec_proxy_numeric <- function(x) {
  UseMethod("vec_proxy_numeric")
}

#' @export
vec_proxy_numeric.default <- function(x) {
  # Same rules as base
  if (is_logical(x) || is_integer(x) || is_double(x)) {
    vec_data(x)
  } else {
    stop_unsupported(x, "numeric-ise")
  }
}

#' @export
#' @rdname vec_proxy_numeric
vec_restore_numeric <- function(x, to) {
  UseMethod("vec_restore_numeric", to)
}

#' @export
vec_restore_numeric.default <- function(x, to) {
  x
}
