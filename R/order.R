#' @export
#' @rdname vec_order_base
vec_sort <- function(x,
                     direction = c("asc", "desc"),
                     na_value = c("largest", "smallest")) {
  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  # TODO: vec_order_base -> vec_order + rdname change
  idx <- vec_order_base(x, direction = direction, na_value = na_value)
  vec_slice(x, idx)
}

# ------------------------------------------------------------------------------

#' Order vectors
#'
#' @description
#' `vec_order_base()` orders vectors using [base::order()], but can handle
#' more complex types, like data frames and [`vctrs_vctr`][vctr] objects, using
#' vctrs principles.
#'
#' `vec_order_base()` is mainly provided for backwards compatibility with vctrs
#' <= 0.3.7. New code should instead use [vec_order()], which has more
#' capabilities. The main difference between the two is that `vec_order()`
#' orders character vectors in the C locale (which is highly performance), while
#' `vec_order_base()` respects the system locale.
#'
#' @param x A vector
#' @param direction Direction to sort in. Defaults to `asc`ending.
#' @param na_value Should `NA`s be treated as the largest or smallest values?
#' @return An integer vector the same size as `x`.
#'
#' @section Differences with `order()`:
#' Unlike the `na.last` argument of `order()` which decides the positions of
#' missing values irrespective of the `decreasing` argument, the `na_value`
#' argument of `vec_order_base()` interacts with `direction`. If missing values
#' are considered the largest value, they will appear last in ascending order,
#' and first in descending order.
#'
#' @section Dependencies of `vec_order_base()`:
#' * [vec_proxy_order()]
#'
#' @export
#' @keywords internal
#' @examples
#' x <- round(c(runif(9), NA), 3)
#' vec_order_base(x)
#' vec_order_base(x, "desc")
#'
#' # Can also handle data frames
#' df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
#' vec_order_base(df)
#' vec_order_base(df, "desc")
#'
#' # Missing values interpreted as largest values are last when
#' # in increasing order:
#' vec_order_base(c(1, NA), na_value = "largest", direction = "asc")
#' vec_order_base(c(1, NA), na_value = "largest", direction = "desc")
vec_order_base <- function(x,
                           direction = c("asc", "desc"),
                           na_value = c("largest", "smallest")) {
  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  decreasing <- !identical(direction, "asc")
  na.last <- identical(na_value, "largest")
  if (decreasing) {
    na.last <- !na.last
  }

  proxy <- vec_proxy_order(x)

  if (is.data.frame(proxy)) {
    # Work around type-instability in `base::order()`
    if (vec_size(proxy) == 0L) {
      return(integer(0L))
    }
    args <- map(unstructure(proxy), function(.x) {
      if (is.data.frame(.x)) {
        .x <- order(vec_order_base(.x, direction = direction, na_value = na_value))
      }
      .x
    })
    exec("order", !!!args, decreasing = decreasing, na.last = na.last)
  } else if (is_character(proxy) || is_logical(proxy) || is_integer(proxy) || is_double(proxy) || is.complex(proxy)) {
    if (is.object(proxy)) {
      proxy <- unstructure(proxy)
    }
    order(proxy, decreasing = decreasing, na.last = na.last)
  } else {
    abort("Invalid type returned by `vec_proxy_order()`.")
  }
}
