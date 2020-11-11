#' Fill in missing values with the previous or following value
#'
#' `vec_fill_missing()` fills gaps of missing values with the previous or
#' following non-missing value.
#'
#' @param x A vector
#' @param direction Direction in which to fill missing values. Must be either
#'   `"down"`, `"up"`, `"downup"`, or `"updown"`.
#' @param max_gap A single positive integer specifying the maximum size of the
#'   gap of sequential missing values that will be filled. If `NULL`, there is
#'   no limit to the gap size.
#'
#' @keywords internal
vec_fill_missing <- function(x,
                             direction = c("down", "up", "downup", "updown"),
                             max_gap = NULL) {
  .Call(vctrs_fill_missing, x, direction, max_gap)
}
