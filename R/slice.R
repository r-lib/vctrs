#' Get or set observations in a vector
#'
#' This provides a common interface to extracting and modifying observations
#' for all vector types, regardless of dimensionality. It is an analog to `[`
#' that matches [vec_size()] instead of `length()`.
#'
#' @param x A vector
#' @param i An integer or character vector specifying the positions or
#'   names of the observations to get/set.
#' @param value Replacement values.
#'
#' @details
#'
#' * `vec_slice()` is an S3 generic for which you can implement methods.
#'   The default method calls `[`.
#'
#' * [vec_restore()] is called on the slice vector to restore
#'   the class and attributes.
#'
#' @section Differences with base R subsetting:
#'
#' * `vec_slice()` only slices along one dimension. For
#'   two-dimensional types, the first dimension is subsetted.
#'
#' * `vec_slice()` preserves attributes by default.
#'
#' @export
#' @keywords internal
#' @examples
#' x <- sample(10)
#' x
#' vec_slice(x, 1:3)
#' vec_slice(x, 2L) <- 100
#' x
#'
#' vec_slice(mtcars, 1:3)
vec_slice <- function(x, i = NULL) {
  .Call(vctrs_slice, x, i, FALSE)
}

# Called when `x` has dimensions
vec_slice_fallback <- function(x, i) {
  out <- unclass(vec_proxy(x))
  vec_assert(out)

  d <- vec_dims(out)
  if (d == 2) {
    out <- out[i, , drop = FALSE]
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    out <- eval_bare(expr(out[i, !!!miss_args, drop = FALSE]))
  }

  vec_restore(out, x)
}

# No dispatch on `[`, should be called in `[` methods
vec_slice_native <- function(x, i) {
  .Call(vctrs_slice, x, i, TRUE)
}

#' @export
#' @rdname vec_slice
`vec_slice<-` <- function(x, i = NULL, value) {
  if (is_null(x)) {
    return(x)
  }

  vec_assert(x)

  i <- vec_as_index(i, x)
  value <- vec_recycle(value, vec_size(i))

  existing <- !is.na(i)
  i <- vec_slice(i, existing)
  value <- vec_slice(value, existing)

  d <- vec_dims(x)
  if (d == 1) {
    x[i] <- value
  } else if (d == 2) {
    x[i, ] <- value
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    eval_bare(expr(x[i, !!!miss_args] <- value))
  }

  x
}

vec_as_index <- function(i, x) {
  .Call(vctrs_as_index, i, x)
}

#' Create a missing vector
#'
#' @param x Template of missing vector
#' @param n Desired size of result
#' @export
#' @examples
#' vec_na(1:10, 3)
#' vec_na(Sys.Date(), 5)
#' vec_na(mtcars, 2)
vec_na <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}
