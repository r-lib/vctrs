#' Runs
#'
#' @description
#' - `vec_identify_runs()` returns a vector of identifiers for the elements of
#'   `x` that indicate which run of repeated values they fall in. The number of
#'   runs is also returned as an attribute, `n`.
#'
#' - `vec_run_sizes()` returns an integer vector corresponding to the size of
#'   each run. This is identical to the `times` column from `vec_unrep()`, but
#'   is faster if you don't need the run keys.
#'
#' - [vec_unrep()] is a generalized [base::rle()]. It is documented alongside
#'   the "repeat" functions of [vec_rep()] and [vec_rep_each()]; look there for
#'   more information.
#'
#' @details
#' Unlike [base::rle()], adjacent missing values are considered identical when
#' constructing runs. For example, `vec_identify_runs(c(NA, NA))` will return
#' `c(1, 1)`, not `c(1, 2)`.
#'
#' @param x A vector.
#'
#' @return
#' - For `vec_identify_runs()`, an integer vector with the same size as `x`. A
#'   scalar integer attribute, `n`, is attached.
#'
#' - For `vec_run_sizes()`, an integer vector with size equal to the number of
#'   runs in `x`.
#'
#' @seealso
#' [vec_unrep()] for a generalized [base::rle()].
#'
#' @name runs
#' @examples
#' x <- c("a", "z", "z", "c", "a", "a")
#'
#' vec_identify_runs(x)
#' vec_run_sizes(x)
#' vec_unrep(x)
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
#' vec_run_sizes(df)
#' vec_unrep(df)
NULL

#' @rdname runs
#' @export
vec_identify_runs <- function(x) {
  .Call(ffi_vec_identify_runs, x, environment())
}

#' @rdname runs
#' @export
vec_run_sizes <- function(x) {
  .Call(ffi_vec_run_sizes, x, environment())
}

vec_locate_run_bounds <- function(x, which = c("start", "end")) {
  .Call(ffi_vec_locate_run_bounds, x, which, environment())
}

vec_detect_run_bounds <- function(x, which = c("start", "end")) {
  .Call(ffi_vec_detect_run_bounds, x, which, environment())
}
