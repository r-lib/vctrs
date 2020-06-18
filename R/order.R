#' Order and sort vectors
#'
#' @param x A vector
#' @param direction Direction to sort in. Defaults to `asc`ending.
#' @param na_value Should `NA`s be treated as the largest or smallest values?
#' @return
#' * `vec_order()` an integer vector the same size as `x`.
#' * `vec_sort()` a vector with the same size and type as `x`.
#'
#' @section Dependencies of `vec_order()`:
#' * [vec_proxy_compare()]
#'
#' @section Dependencies of `vec_sort()`:
#' * [vec_proxy_compare()]
#' * [vec_order()]
#' * [vec_slice()]
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
vec_order <- function(x, direction = "asc", na_value = "largest") {
  .Call(vctrs_order, x, direction, na_value)
}

vec_order_groups <- function(x, direction = "asc", na_value = "largest") {
  .Call(vctrs_order_groups, x, direction, na_value)
}

#' @export
#' @rdname vec_order
vec_sort <- function(x, direction = "asc", na_value = "largest") {
  idx <- vec_order(x, direction = direction, na_value = na_value)
  vec_slice(x, idx)
}
