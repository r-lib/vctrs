#' Equality and ordering proxies
#'
#' `vec_proxy_equality()` powers `==` and the hashing functions (and which
#' powers `vec_duplicated()` etc).
#' `vec_proxy_order()` powers `order()`, `sort()`, `<`, `>`, and so on.
#' Override these methods if your class has different equality or ordering
#' semantics.
#'
#' @keywords internal
#' @param x A vector x.
#' @return A 1d atomic vector, 1d list, or a data frame. For these functions,
#'   `NA`s (and `NaN`s) are considered to be equal, and smaller than all
#'   other values (i.e. they're equivalent to `-Inf`).
#' @name vec_proxy
NULL

#' @export
#' @rdname vec_proxy
vec_proxy_equality <- function(x) {
  UseMethod("vec_proxy_equality")
}

#' @export
vec_proxy_equality.data.frame <- function(x) {
  x
}

#' @export
vec_proxy_equality.POSIXlt <- function(x) {
  new_data_frame(vec_data(x), length(x))
}

#' @export
vec_proxy_equality.default <- function(x) {
  vec_data(x)
}

#' @export
#' @rdname vec_proxy
vec_proxy_order <- function(x) {
  UseMethod("vec_proxy_order")
}

#' @export
vec_proxy_order.vctr <- function(x) {
  if (is.list(x)) {
    stop_unimplemented(x, "vec_proxy_order")
  } else {
    order(vec_data(x))
  }
}
