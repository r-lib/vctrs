#' Complete
#'
#' @description
#' `vec_detect_complete()` detects "complete" observations. An observation is
#' considered complete if it is non-missing. For most vectors, this implies that
#' `vec_detect_complete(x) == !vec_equal_na(x)`.
#'
#' For data frames and matrices, a row is only considered complete if all
#' elements of that row are non-missing. To compare, `!vec_equal_na(x)` detects
#' rows that are partially complete (they have at least one non-missing value).
#'
#' @details
#' A [record][new_rcrd] type vector is similar to a data frame, and is only
#' considered complete if all fields are non-missing.
#'
#' @param x A vector
#'
#' @return
#' A logical vector with the same size as `x`.
#'
#' @seealso [stats::complete.cases()]
#' @export
#' @examples
#' x <- c(1, 2, NA, 4, NA)
#'
#' # For most vectors, this is identical to `!vec_equal_na(x)`
#' vec_detect_complete(x)
#' !vec_equal_na(x)
#'
#' df <- data_frame(
#'   x = x,
#'   y = c("a", "b", NA, "d", "e")
#' )
#'
#' # This returns `TRUE` where all elements of the row are non-missing.
#' # Compare that with `!vec_equal_na()`, which detects rows that have at
#' # least one non-missing value.
#' df2 <- df
#' df2$all_non_missing <- vec_detect_complete(df)
#' df2$any_non_missing <- !vec_equal_na(df)
#' df2
vec_detect_complete <- function(x) {
  .Call(vctrs_detect_complete, x)
}

vec_slice_complete <- function(x) {
  .Call(vctrs_slice_complete, x)
}

vec_locate_complete <- function(x) {
  .Call(vctrs_locate_complete, x)
}
