#' Runs
#'
#' @description
#' - `vec_identify_runs()` returns a vector of identifiers for the elements of
#'   `x` that indicate which run of repeated values they fall in. The number of
#'   runs is also returned as an attribute, `n`.
#'
#' - `vec_locate_runs()` returns a vector of locations corresponding to either
#'   the starting or ending location of each run, depending on the value of
#'   `start`.
#'
#' @details
#' Unlike [base::rle()], adjacent missing values are considered identical when
#' constructing runs. For example, `vec_identify_runs(c(NA, NA))` will return
#' `c(1, 1)`, not `c(1, 2)`.
#'
#' @param x A vector.
#'
#' @param start A single logical specifying whether the start or end of each
#'   run should be detected.
#'
#' @return
#' - `vec_identify_runs()`: An integer vector with the same size as `x`. A
#'   scalar integer attribute, `n`, is attached.
#'
#' - `vec_locate_runs()`: An integer vector with a size equal to the number
#'   of runs in `x`.
#'
#' @export
#' @examples
#' x <- c("a", "z", "z", "c", "a", "a")
#'
#' vec_identify_runs(x)
#'
#' # Starting location of each run
#' vec_locate_runs(x)
#'
#' # Ending location of each run
#' vec_locate_runs(x, start = FALSE)
#'
#' y <- c(1, 1, 1, 2, 2, 3)
#'
#' # With multiple columns, the runs are constructed rowwise
#' df <- data_frame(
#'   x = x,
#'   y = y
#' )
#'
#' vec_identify_runs(df)
vec_identify_runs <- function(x) {
  .Call(vctrs_identify_runs, x)
}

#' @rdname vec_identify_runs
#' @export
vec_locate_runs <- function(x, start = TRUE) {
  .Call(vctrs_locate_runs, x, start)
}
