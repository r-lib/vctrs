#' Missing values
#'
#' @description
#' - `vec_detect_missing()` returns a logical vector the same size as `x`. For
#' each element of `x`, it returns `TRUE` if the element is missing, and `FALSE`
#' otherwise.
#'
#' - `vec_any_missing()` returns a single `TRUE` or `FALSE` depending on whether
#' or not `x` has _any_ missing values.
#'
#' ## Differences with [is.na()]
#'
#' Data frame rows are only considered missing if every element in the row is
#' missing. Similarly, [record vector][new_rcrd()] elements are only considered
#' missing if every field in the record is missing. Put another way, rows with
#' _any_ missing values are considered [incomplete][vec_detect_complete()], but
#' only rows with _all_ missing values are considered missing.
#'
#' List elements are only considered missing if they are `NULL`.
#'
#' @param x A vector
#'
#' @return
#' - `vec_detect_missing()` returns a logical vector the same size as `x`.
#'
#' - `vec_any_missing()` returns a single `TRUE` or `FALSE`.
#'
#' @section Dependencies:
#' - [vec_proxy_equal()]
#'
#' @name missing
#' @seealso [vec_detect_complete()]
#'
#' @examples
#' x <- c(1, 2, NA, 4, NA)
#'
#' vec_detect_missing(x)
#' vec_any_missing(x)
#'
#' # Data frames are iterated over rowwise, and only report a row as missing
#' # if every element of that row is missing. If a row is only partially
#' # missing, it is said to be incomplete, but not missing.
#' y <- c("a", "b", NA, "d", "e")
#' df <- data_frame(x = x, y = y)
#'
#' df$missing <- vec_detect_missing(df)
#' df$incomplete <- !vec_detect_complete(df)
#' df
NULL

#' Missing proxy
#'
#' Returns a proxy object (i.e. an atomic vector or data frame of atomic
#' vectors). For [vctr]s, this determines the behaviour of
#' [is.na()] and [anyNA()] (via [vec_detect_missing()]).
#'
#' The default method calls [vec_proxy_equal()], as the default
#' equal-able proxy should be used to detect missingness in most cases.
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
#' - [vec_proxy_equal()] called by default
#'
#' @export
vec_proxy_missing <- function(x, ...) {
  check_dots_empty0(...)
  return(.Call(vctrs_proxy_missing, x))
  UseMethod("vec_proxy_missing")
}

#' @export
vec_proxy_missing.default <- function(x, ...) {
  stop_native_implementation("vec_proxy_missing.default")
}

#' @rdname missing
#' @export
vec_detect_missing <- function(x) {
  .Call(ffi_vec_detect_missing, x)
}

#' @rdname missing
#' @export
vec_any_missing <- function(x) {
  .Call(ffi_vec_any_missing, x)
}
