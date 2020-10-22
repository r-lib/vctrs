#' Fill in missing values with the previous or following value
#'
#' `vec_fill()` fills gaps of missing values with the previous or following
#' non-missing value.
#'
#' @param x A vector
#' @param direction Direction in which to fill missing values. Must be either
#'   `"down"` or `"up"`.
#' @param leading A logical specifying if leading missing values should be
#'   back-filled with the first non-missing value found in the chosen
#'   `direction`.
#' @param max_gap A single positive integer specifying the maximum size of the
#'   gap of sequential missing values that will be filled. If `NULL`, there is
#'   no limit to the gap size.
#'
#' @keywords internal
vec_fill <- function(x,
                     direction = c("down", "up"),
                     leading = FALSE,
                     max_gap = NULL) {
  .Call(vctrs_fill, x, direction, leading, max_gap)
}
