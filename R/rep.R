#' Repeat a vector
#'
#' @description
#' - `vec_rep()` repeats an entire vector a set number of `times`.
#'
#' - `vec_rep_each()` repeats each element of a vector a set number of `times`.
#'
#' - `vec_unrep()` compresses a vector with repeated values. The repeated values
#'   are returned as a `key` alongside the number of `times` each key is
#'   repeated.
#'
#' @details
#' These functions work along the size of `x`, rather than its length. For data
#' frames, this means that rows are repeated rather than columns.
#'
#' Using `vec_unrep()` and `vec_rep_each()` together is very similar to using
#' [base::rle()] and [base::inverse.rle()]. The following invariant shows
#' the relationship between the two functions:
#'
#' ```
#' compressed <- vec_unrep(x)
#' identical(x, vec_rep_each(compressed$key, compressed$times))
#' ```
#'
#' @param x A vector.
#' @param times
#'   For `vec_rep()`, a single integer for the number of times to repeat
#'   the entire vector.
#'
#'   For `vec_rep_each()`, an integer vector of the number of times to repeat
#'   each element of `x`. `times` will be recycled to the size of `x`.
#'
#' @return
#' For `vec_rep()`, a vector the same type as `x` with size
#' `vec_size(x) * times`.
#'
#' For `vec_rep_each()`, a vector the same type as `x` with size
#' `sum(vec_recycle(times, vec_size(x)))`.
#'
#' For `vec_unrep()`, a data frame with two columns, `key` and `times`. `key`
#' is a vector with the same type as `x`, and `times` is an integer vector.
#'
#' @section Dependencies:
#' - [vec_slice()]
#'
#' @name vec-rep
#' @examples
#' # Repeat the entire vector
#' vec_rep(1:2, 3)
#'
#' # Repeat within each vector
#' vec_rep_each(1:2, 3)
#' x <- vec_rep_each(1:2, c(3, 4))
#' x
#'
#' # After using `vec_rep_each()`, you can recover the original vector
#' # with `vec_unrep()`
#' vec_unrep(x)
#'
#' df <- data.frame(x = 1:2, y = 3:4)
#'
#' # `rep()` repeats columns of data frames, and returns lists
#' rep(df, each = 2)
#'
#' # `vec_rep()` and `vec_rep_each()` repeat rows, and return data frames
#' vec_rep(df, 2)
#' vec_rep_each(df, 2)
NULL

#' @rdname vec-rep
#' @export
vec_rep <- function(x, times) {
  .Call(vctrs_rep, x, times)
}

#' @rdname vec-rep
#' @export
vec_rep_each <- function(x, times) {
  .Call(vctrs_rep_each, x, times)
}

#' @rdname vec-rep
#' @export
vec_unrep <- function(x) {
  .Call(vctrs_unrep, x)
}
