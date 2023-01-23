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
#' Using `vec_unrep()` and `vec_rep_each()` together is similar to using
#' [base::rle()] and [base::inverse.rle()]. The following invariant shows
#' the relationship between the two functions:
#'
#' ```
#' compressed <- vec_unrep(x)
#' identical(x, vec_rep_each(compressed$key, compressed$times))
#' ```
#'
#' There are two main differences between `vec_unrep()` and [base::rle()]:
#'
#' - `vec_unrep()` treats adjacent missing values as equivalent, while `rle()`
#'   treats them as different values.
#'
#' - `vec_unrep()` works along the size of `x`, while `rle()` works along its
#'   length. This means that `vec_unrep()` works on data frames by compressing
#'   repeated rows.
#'
#' @inheritParams rlang::args_error_context
#' @inheritParams rlang::args_dots_empty
#' @param x A vector.
#' @param times
#'   For `vec_rep()`, a single integer for the number of times to repeat
#'   the entire vector.
#'
#'   For `vec_rep_each()`, an integer vector of the number of times to repeat
#'   each element of `x`. `times` will be [recycled][vector_recycling_rules] to
#'   the size of `x`.
#' @param x_arg,times_arg Argument names for errors.
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
#'
#' # `rle()` treats adjacent missing values as different
#' y <- c(1, NA, NA, 2)
#' rle(y)
#'
#' # `vec_unrep()` treats them as equivalent
#' vec_unrep(y)
NULL

#' @rdname vec-rep
#' @export
vec_rep <- function(x,
                    times,
                    ...,
                    error_call = current_env(),
                    x_arg = "x",
                    times_arg = "times") {
  check_dots_empty0(...)
  .Call(ffi_vec_rep, x, times, environment())
}

#' @rdname vec-rep
#' @export
vec_rep_each <- function(x,
                         times,
                         ...,
                         error_call = current_env(),
                         x_arg = "x",
                         times_arg = "times") {
  check_dots_empty0(...)
  .Call(ffi_vec_rep_each, x, times, environment())
}

#' @rdname vec-rep
#' @export
vec_unrep <- function(x) {
  .Call(ffi_vec_unrep, x, environment())
}
