#' Drop empty elements from a list
#'
#' `list_drop_empty()` removes empty elements from a list. This includes `NULL`
#' elements along with empty vectors, like `integer(0)`. This is equivalent to,
#' but faster than, `vec_slice(x, list_sizes(x) != 0L)`.
#'
#' @section Dependencies:
#' - [vec_slice()]
#'
#' @param x A list.
#'
#' @export
#' @examples
#' x <- list(1, NULL, integer(), 2)
#' list_drop_empty(x)
list_drop_empty <- function(x) {
  .Call(vctrs_list_drop_empty, x)
}
