#' Comparison proxy
#'
#' Returns a proxy object (i.e. an atomic vector or data frame of atomic
#' vectors). For [vctr]s, this determins the behaviour of [order()] and
#' [sort()] (via [xtfrm()]); `<`, `>`, `>=` and `<=` (via [vec_compare()]);
#' and [min()], [max()], [median()], and [quantile()].
#'
#' The default method assumes that all classes built on top of atomic
#' vectors or records are orderable. If your class is not, you will need
#' to provide a `vec_proxy_compare()` method that throws an error. Note
#' that the default [vec_proxy_equal()] method calls `vec_proxy_compare()` so
#' if your object is equal-able but not comparable, you'll need to provide
#' methods for both generics.
#'
#' @param x A vector x.
#' @return A 1d atomic vector or a data frame.
#' @keywords internal
#' @export
vec_proxy_compare <- function(x) {
  UseMethod("vec_proxy_compare")
}

#' @export
vec_proxy_compare.data.frame <- function(x) {
  is_list <- map_lgl(x, is.list)
  x[is_list] <- lapply(x[is_list], vec_proxy_compare)
  x
}

#' @export
vec_proxy_compare.POSIXlt <- function(x) {
  new_data_frame(vec_data(x), n = length(x))
}

#' @export
vec_proxy_compare.default <- function(x) {
  if (is_bare_list(x)) {
    stop_unsupported(x, "vec_proxy_compare")
  } else {
    vec_data(x)
  }
}

#' @export
vec_proxy_compare.NULL <- function(x) {
  NULL
}

#' Compare two vectors
#'
#' @section S3 dispatch:
#' `vec_compare()` is not generic for performance; instead it uses
#' [vec_proxy_compare()] to
#'
#' @param x,y Vectors with compatible types and lengths.
#' @param na_equal Should `NA` values be considered equal?
#' @param .ptype Override to optionally specify common type
#' @return An integer vector with values -1 for `x < y`, 0 if `x == y`,
#'    and 1 if `x > y`. If `na_equal` is `FALSE`, the result will be `NA`
#'    if either `x` or `y` is `NA`.
#' @export
#' @examples
#' vec_compare(c(TRUE, FALSE, NA), FALSE)
#' vec_compare(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#'
#' vec_compare(1:10, 5)
#' vec_compare(runif(10), 0.5)
#' vec_compare(letters[1:10], "d")
#'
#' df <- data.frame(x = c(1, 1, 1, 2), y = c(0, 1, 2, 1))
#' vec_compare(df, data.frame(x = 1, y = 1))
vec_compare <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  args <- vec_recycle_common(x, y)
  args <- vec_cast_common(!!!args, .to = .ptype)
  .Call(vctrs_compare, vec_proxy_equal(args[[1]]), vec_proxy_equal(args[[2]]), na_equal)
}

# Helpers -----------------------------------------------------------------

# Used for testing
cmp <- function(x, y) (x > y) - (x < y)
