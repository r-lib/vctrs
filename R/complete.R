#' Complete
#'
#' @description
#' These functions are for working with "complete" values. For atomic vectors,
#' this is the same as `!vec_equal_na(x)`. For data frames, a row is only
#' considered complete if all elements of that row are non-missing.
#' To compare, `!vec_equal_na(x)` for data frames detects rows that have at
#' least one non-missing value.
#'
#' - `vec_slice_complete()` returns a vector with the same type as `x` holding
#'   the complete elements.
#'
#' - `vec_locate_complete()` returns a vector of locations corresponding to
#'   the complete elements of `x`.
#'
#' - `vec_detect_complete()` returns a logical vector that detects if elements
#'   of `x` are complete.
#'
#' @param x A vector
#'
#' @return
#' - `vec_slice_complete()`: A vector with the same type as `x`. The size
#'   of the result is equal to the number of complete elements.
#'
#' - `vec_locate_complete()` An integer vector. The size
#'   of the result is equal to the number of complete elements.
#'
#' - `vec_detect_complete()` A logical vector with the same size as `x`.
#'
#' @name vec-complete
#' @seealso [stats::complete.cases()]
#' @export
#' @examples
#' x <- c(1, 2, NA, 4, NA)
#'
#' # For atomic vectors, this is the same as `!vec_equal_na()`
#' vec_detect_complete(x)
#' !vec_equal_na(x)
#'
#' y <- c("a", "b", NA, "d", "e")
#'
#' df <- data_frame(x = x, y = y)
#'
#' # For data frames, this returns `TRUE` where all elements
#' # of the row are non-missing. Compare that with `!vec_equal_na()`,
#' # which detects rows that have at least one non-missing value.
#' df2 <- df
#' df2$all_non_missing <- vec_detect_complete(df)
#' df2$any_non_missing <- !vec_equal_na(df)
#'
#' # Extract only the complete rows with `vec_slice_complete()`
#' vec_slice_complete(df)
vec_slice_complete <- function(x) {
  .Call(vctrs_slice_complete, x)
}

#' @rdname vec-complete
#' @export
vec_locate_complete <- function(x) {
  .Call(vctrs_locate_complete, x)
}

#' @rdname vec-complete
#' @export
vec_detect_complete <- function(x) {
  .Call(vctrs_detect_complete, x)
}
