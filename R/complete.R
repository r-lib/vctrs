#' Complete
#'
#' @description
#' These functions are for working with "complete" values. For atomic vectors,
#' this is the same as `!vec_equal_na(x)`. For data frames, a row is only
#' considered complete if all elements of that row are non-missing.
#' To compare, `!vec_equal_na(x)` for data frames detects rows that have at
#' least one non-missing value.
#'
#' - `df_slice_complete()` returns a vector with the same type as `x` holding
#'   the complete elements.
#'
#' - `df_locate_complete()` returns a vector of locations corresponding to
#'   the complete elements of `x`.
#'
#' - `df_detect_complete()` returns a logical vector that detects if elements
#'   of `x` are complete.
#'
#' @param x A vector
#'
#' @return
#' - `df_slice_complete()`: A vector with the same type as `x`. The size
#'   of the result is equal to the number of complete elements.
#'
#' - `df_locate_complete()` An integer vector. The size
#'   of the result is equal to the number of complete elements.
#'
#' - `df_slice_complete()` A logical vector with the same size as `x`.
#'
#' @name vec-complete
#' @seealso [stats::complete.cases()]
#' @export
#' @examples
#' df <- data_frame(x = c(1, 2, NA, 4, NA), y = c("a", "b", NA, "d", "e"))
#'
#' # This returns `TRUE` where all elements of the row are non-missing.
#' # Compare that with `!vec_equal_na()`, which detects rows that have at
#' # least one non-missing value.
#' df2 <- df
#' df2$all_non_missing <- df_detect_complete(df)
#' df2$any_non_missing <- !vec_equal_na(df)
#' df2
#'
#' # Extract only the complete rows with `df_slice_complete()`
#' df_slice_complete(df)
df_slice_complete <- function(x) {
  .Call(vctrs_df_slice_complete, x)
}

#' @rdname vec-complete
#' @export
df_locate_complete <- function(x) {
  .Call(vctrs_df_locate_complete, x)
}

#' @rdname vec-complete
#' @export
df_detect_complete <- function(x) {
  .Call(vctrs_df_detect_complete, x)
}
