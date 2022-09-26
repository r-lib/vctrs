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
