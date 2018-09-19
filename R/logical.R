#' Logical proxy
#'
#' `vec_proxy_logical()` converts an object to it's logical representation
#' (if available). `vec_restore_logical()` restores a logical proxy back to
#' the original type. The default methods allow any object built on a logical,
#' integer, or numeric, to be coerced to logical, and leave the result as is.
#'
#' @keywords internal
#' @export
#' @examples
#' x <- new_vctr(c(TRUE, FALSE, NA))
#' y <- new_vctr(c(TRUE, TRUE, TRUE))
#'
#' !x
#' x | y
#' x & y
vec_proxy_logical <- function(x) {
  UseMethod("vec_proxy_logical")
}

#' @export
vec_proxy_logical.default <- function(x) {
  # Same rules as base
  if (is_logical(x) || is_integer(x) || is_double(x)) {
    vec_data(x)
  } else {
    stop("Non-logical input", call. = FALSE)
  }
}

#' @export
#' @rdname vec_proxy_logical
vec_restore_logical <- function(x, to) {
  UseMethod("vec_restore_logical", to)
}

#' @export
vec_restore_logical.default <- function(x, to) {
  x
}
