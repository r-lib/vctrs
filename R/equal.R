#' Equality proxy
#'
#' Returns a proxy object (i.e. an atomic vector or data frame of atomic
#' vectors). For [vctr]s, this determines the behaviour of `==` and
#' `!=` (via [vec_equal()]); [unique()], [duplicated()] (via
#' [vec_unique()] and [vec_duplicate_detect()]); [is.na()] and [anyNA()]
#' (via [vec_detect_missing()]).
#'
#' The default method calls [vec_proxy()], as the default underlying
#' vector data should be equal-able in most cases. If your class is
#' not equal-able, provide a `vec_proxy_equal()` method that throws an
#' error.
#'
#' @section Data frames:
#' If the proxy for `x` is a data frame, the proxy function is automatically
#' recursively applied on all columns as well. After applying the proxy
#' recursively, if there are any data frame columns present in the proxy, then
#' they are unpacked. Finally, if the resulting data frame only has a single
#' column, then it is unwrapped and a vector is returned as the proxy.
#'
#' @param x A vector x.
#' @inheritParams rlang::args_dots_empty
#'
#' @return A 1d atomic vector or a data frame.
#' @keywords internal
#'
#' @section Dependencies:
#' - [vec_proxy()] called by default
#'
#' @export
vec_proxy_equal <- function(x, ...) {
  check_dots_empty0(...)
  return(.Call(vctrs_proxy_equal, x))
  UseMethod("vec_proxy_equal")
}
#' @export
vec_proxy_equal.default <- function(x, ...) {
  stop_native_implementation("vec_proxy_equal.default")
}

#' Equality
#'
#' `vec_equal()` tests if two vectors are equal.
#'
#' @inheritParams vec_compare
#' @return A logical vector the same size as the common size of `x` and `y`.
#'   Will only contain `NA`s if `na_equal` is `FALSE`.
#'
#' @section Dependencies:
#' - [vec_cast_common()] with fallback
#' - [vec_recycle_common()]
#' - [vec_proxy_equal()]
#'
#' @seealso [vec_detect_missing()]
#'
#' @export
#' @examples
#' vec_equal(c(TRUE, FALSE, NA), FALSE)
#' vec_equal(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#'
#' vec_equal(5, 1:10)
#' vec_equal("d", letters[1:10])
#'
#' df <- data.frame(x = c(1, 1, 2, 1), y = c(1, 2, 1, NA))
#' vec_equal(df, data.frame(x = 1, y = 2))
vec_equal <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  vec_assert(na_equal, ptype = logical(), size = 1L)
  args <- vec_recycle_common(x, y)
  args <- vec_cast_common_params(!!!args, .to = .ptype)
  .Call(vctrs_equal, args[[1]], args[[2]], na_equal)
}

obj_equal <- function(x, y) {
  .Call(vctrs_equal_object, x, y)
}
