#' Modify a function to act on a deduplicated vector input
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' The deduplicated function acts on the unique values in the first input `x`
#' and expands the output back to return. The return value is equivalent to `f(x)`
#' but is significantly faster for inputs with significant duplication.
#'
#' @param f Function whose first argument will be deduplicated.
#'
#' @return A deduplicated function
#' @export
#'
#' @examples
#' x <- sample(LETTERS, 10)
#' x
#'
#' large_x <- sample(rep(x, 10))
#' length(large_x)
#'
#' long_func <- function(x) for(i in x) {Sys.sleep(0.001)}
#'
#' system.time(y <- long_func(large_x))
#' system.time(y2 <- vec_deduplicate(long_func)(large_x))
#' all(y == y2)
vec_deduplicate <- function(f) {
  function(x, ...) {
    res <- vec_group_id_and_loc(x)
    group_id <- unclass(res)
    unique_loc <- attr(res, "unique_loc")
    unique_x <- vec_slice(x, unique_loc)
    f(unique_x, ...)[group_id]
  }
}

vec_group_id_and_loc <- function(x) {
  .Call(vctrs_group_id_and_loc, x)
}
