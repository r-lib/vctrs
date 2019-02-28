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


# order/sort --------------------------------------------------------------

#' Order and sort vectors
#'
#' @param x A vector
#' @param direction Direction to sort in. Defaults to `asc`ending.
#' @param na_value Should `NA`s be treated as the largest or smallest values?
#' @return
#' * `vec_order()` an integer vector the same size as `x`.
#' * `vec_sort()` a vector with the same size and type as `x`.
#' @export
#' @examples
#' x <- round(c(runif(9), NA), 3)
#' vec_order(x)
#' vec_sort(x)
#' vec_sort(x, "desc")
#'
#' # Can also handle data frames
#' df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
#' vec_order(df)
#' vec_sort(df)
#' vec_sort(df, "desc")
vec_order <- function(x,
                      direction = c("asc", "desc"),
                      na_value = c("largest", "smallest")
                      ) {
  direction <- match.arg(direction)
  na_value <- match.arg(na_value)

  order_proxy(vec_proxy_compare(x), direction = direction, na_value = na_value)
}

#' @export
#' @rdname vec_order
vec_sort <- function(x,
                     direction = c("asc", "desc"),
                     na_value = c("largest", "smallest")) {
  direction <- match.arg(direction)
  na_value <- match.arg(na_value)

  idx <- vec_order(x, direction = direction, na_value = na_value)
  vec_slice(x, idx)
}

order_proxy <- function(proxy, direction = "asc", na_value = "largest") {
  decreasing <- !identical(direction, "asc")
  na.last <- identical(na_value, "largest")
  if (decreasing) {
    na.last <- !na.last
  }

  if (is.data.frame(proxy)) {
    args <- unname(proxy)
    args$decreasing <- decreasing
    args$na.last <- na.last

    do.call(base::order, args)
  } else if (is_character(proxy) || is_logical(proxy) || is_integer(proxy) || is_double(proxy)) {
    order(proxy, decreasing = decreasing, na.last = na.last)
  } else {
    abort("Invalid type returned by `vec_proxy_compare()`.")
  }
}

# Helpers -----------------------------------------------------------------

# Used for testing
cmp <- function(x, y) (x > y) - (x < y)
