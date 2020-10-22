#' Complete
#'
#' @description
#' `df_detect_complete()` detects "complete" rows of a data frame. A
#' row is only considered complete if all elements of that row are non-missing.
#' To compare, `!vec_equal_na(x)` detects rows that have at least one
#' non-missing value.
#'
#' @param x A data frame
#'
#' @return
#' A logical vector with the same size as `x`.
#'
#' @seealso [stats::complete.cases()]
#' @export
#' @examples
#' df <- data_frame(
#'   x = c(1, 2, NA, 4, NA),
#'   y = c("a", "b", NA, "d", "e")
#' )
#'
#' # This returns `TRUE` where all elements of the row are non-missing.
#' # Compare that with `!vec_equal_na()`, which detects rows that have at
#' # least one non-missing value.
#' df2 <- df
#' df2$all_non_missing <- df_detect_complete(df)
#' df2$any_non_missing <- !vec_equal_na(df)
#' df2
df_detect_complete <- function(x) {
  .Call(vctrs_df_detect_complete, x)
}

df_slice_complete <- function(x) {
  .Call(vctrs_df_slice_complete, x)
}

df_locate_complete <- function(x) {
  .Call(vctrs_df_locate_complete, x)
}

vec_proxy_complete <- function(x) {
  .Call(vctrs_proxy_complete, x)
}
