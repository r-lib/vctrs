#' Fill in missing values with the previous or following value
#'
#' @description
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' `vec_fill_missing()` fills gaps of missing values with the previous or
#' following non-missing value.
#'
#' @param x A vector
#' @param direction Direction in which to fill missing values. Must be either
#'   `"down"`, `"up"`, `"downup"`, or `"updown"`.
#' @param max_fill A single positive integer specifying the maximum number of
#'   sequential missing values that will be filled. If `NULL`, there is
#'   no limit.
#'
#' @export
#' @examples
#' x <- c(NA, NA, 1, NA, NA, NA, 3, NA, NA)
#'
#' # Filling down replaces missing values with the previous non-missing value
#' vec_fill_missing(x, direction = "down")
#'
#' # To also fill leading missing values, use `"downup"`
#' vec_fill_missing(x, direction = "downup")
#'
#' # Limit the number of sequential missing values to fill with `max_fill`
#' vec_fill_missing(x, max_fill = 1)
#'
#' # Data frames are filled rowwise. Rows are only considered missing
#' # if all elements of that row are missing.
#' y <- c(1, NA, 2, NA, NA, 3, 4, NA, 5)
#' df <- data_frame(x = x, y = y)
#' df
#'
#' vec_fill_missing(df)
vec_fill_missing <- function(x,
                             direction = c("down", "up", "downup", "updown"),
                             max_fill = NULL) {
  .Call(vctrs_fill_missing, x, direction, max_fill)
}
