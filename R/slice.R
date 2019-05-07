#' Get or set observations in a vector
#'
#' This provides a common interface to extracting and modifying observations
#' for all vector types, regardless of dimensionality. It is an analog to `[`
#' that matches [vec_size()] instead of `length()`.
#'
#' @param x A vector
#' @param i An integer, character or logical vector specifying the positions or
#'   names of the observations to get/set.
#'   Specify `TRUE` to index all elements (as in `x[]`), or `NULL`, `FALSE` or
#'   `integer()` to index none (as in `x[NULL]`).
#' @param value Replacement values. `value` is cast to the type of
#'   `x`, but only if they have a common type. See below for examples
#'   of this rule.
#' @return A vector of the same type as `x`.
#'
#' @section Genericity:
#'
#' * If the input is a class instance, `vec_slice()` falls back to the
#'   base generic `[`. Note that S3 lists are treated as scalars by
#'   default, and will cause an error if they don't implement a
#'   [vec_proxy()] method.
#'
#' * The vctrs class methods for `[` call back into `vec_slice()`.
#'   They slice the [vec_proxy()] and [vec_restore()] the result.
#'
#'   The tools for calling back into `vec_slice()` are not exported
#'   yet, but you can take advantage of this mechanism by inheriting
#'   from [vctrs_vctr][new_vctr] or [vctrs_rcrd][new_rcrd].
#'
#' @section Differences with base R subsetting:
#'
#' * `vec_slice()` only slices along one dimension. For
#'   two-dimensional types, the first dimension is subsetted.
#'
#' * `vec_slice()` preserves attributes by default.
#'
#' * `vec_slice<-()` is type-stable and always returns the same type
#'   as the LHS.
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
#'
#'
#' # Type stability --------------------------------------------------
#'
#' # The assign variant is type stable. It always returns the same
#' # type as the input.
#' x <- 1:5
#' vec_slice(x, 2) <- 20.0
#'
#' # `x` is still an integer vector because the RHS was cast to the
#' # type of the LHS:
#' vec_type(x)
#'
#' # Compare to `[<-`:
#' x[2] <- 20.0
#' vec_type(x)
#'
#'
#' # Note that the types must be coercible for the cast to happen.
#' # For instance, you can cast a character vector to an integer:
#' vec_cast("1", integer())
#'
#' # But these types are not coercible:
#' try(vec_type2("1", integer()))
#'
#' # Hence you cannot assign character values to an integer or double
#' # vector:
#' try(vec_slice(x, 2) <- "20")
vec_slice <- function(x, i) {
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
`vec_slice<-` <- function(x, i, value) {
  if (is_null(x)) {
    return(x)
  }

  vec_assert(x)
  value <- vec_coercible_cast(value, x, x_arg = "value", to_arg = "x")

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

vec_index <- function(x, i, ...) {
  i <- maybe_missing(i, TRUE)

  if (!dots_n(...)) {
    return(vec_slice_native(x, i))
  }

  out <- unclass(vec_proxy(x))
  vec_assert(out)

  i <- vec_as_index(i, out)
  vec_restore(out[i, ..., drop = FALSE], x, i = i)
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
