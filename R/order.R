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
#' * [vec_proxy_order()]
#'
#' @section Dependencies of `vec_sort()`:
#' * [vec_proxy_order()]
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
vec_order <- function(x,
                      direction = c("asc", "desc"),
                      na_value = c("largest", "smallest")) {
  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  order_proxy(vec_proxy_order(x), direction = direction, na_value = na_value)
}

#' @export
#' @rdname vec_order
vec_sort <- function(x,
                     direction = c("asc", "desc"),
                     na_value = c("largest", "smallest")) {
  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

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
    # Work around type-instability in `base::order()`
    if (vec_size(proxy) == 0L) {
      return(integer(0L))
    }
    args <- map(unstructure(proxy), function(.x) {
      if (is.data.frame(.x)) {
        .x <- order(vec_order(.x, direction = direction, na_value = na_value))
      }
      .x
    })
    exec("order", !!!args, decreasing = decreasing, na.last = na.last)
  } else if (is_character(proxy) || is_logical(proxy) || is_integer(proxy) || is_double(proxy)) {
    if (is.object(proxy)) {
      proxy <- unstructure(proxy)
    }
    order(proxy, decreasing = decreasing, na.last = na.last)
  } else {
    abort("Invalid type returned by `vec_proxy_compare()`.")
  }
}
