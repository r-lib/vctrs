#' Default value for empty vectors
#'
#' Use this inline operator when you need to provide a default value for
#' empty (as defined by [vec_empty()]) vectors.
#'
#' @param x A vector
#' @param y Value to use to `x` is empty. To preserve type-stability, should
#'   be the same type as `x`.
#' @rdname op-empty-default
#' @export
#' @examples
#' 1:10 %0% 5
#' integer() %0% 5
`%0%` <- function(x, y) {
  if (vec_empty(x)) y else x
}
