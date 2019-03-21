#' Get or set observations in a vector
#'
#' This provides a common interface to extracting and modifying observations
#' for all vector types, regardless of dimensionality. It is an analog to `[`
#' that matches [vec_size()] instead of `length()`.
#'
#' `vec_slice()` is an S3 generic for which you can implement methods.
#' The default method calls `[`.
#'
#' @param x A vector
#' @param i An integer or character vector specifying the positions or
#'   names of the observations to get/set.
#' @param value Replacement values.
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
vec_slice <- function(x, i) {
  return(.Call(vctrs_slice, x, maybe_missing(i), environment()))
  UseMethod("vec_slice")
}
vec_slice_dispatch <- function(x, i) {
  UseMethod("vec_slice")
}
#' @export
vec_slice.default <- function(x, i) {
  if (is.data.frame(x)) {
    # Much faster, and avoids creating rownames
    out <- lapply(x, vec_slice, i)
    attr(out, "row.names") <- .set_row_names(length(i))
    return(vec_restore(out, x))
  }

  vec_assert(x)

  d <- vec_dims(x)
  if (d == 1) {
    if (is.object(x)) {
      x[i]
    } else {
      x[i, drop = FALSE]
    }
  } else if (d == 2) {
    x[i, , drop = FALSE]
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    eval_bare(expr(x[i, !!!miss_args, drop = FALSE]))
  }
}

vec_slice_bare <- function(x, i) {
  .Call(vctrs_slice, x, maybe_missing(i), NULL)
}

#' @export
#' @rdname vec_slice
`vec_slice<-` <- function(x, i, value) {
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
  .Call(vctrs_as_index, maybe_missing(i), x)
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
