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
#' Support for S3 objects depends on whether the object implements a
#' [vec_proxy()] method.
#'
#' * When a `vec_proxy()` method exists, the proxy is sliced and
#'   `vec_restore()` is called on the result.
#'
#' * Otherwise `vec_slice()` falls back to the base generic `[`.
#'
#' Note that S3 lists are treated as scalars by default, and will
#' cause an error if they don't implement a [vec_proxy()] method.
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
#'
#' # You can assign with the infix variant:
#' vec_slice(x, 2) <- 100
#' x
#'
#' # Or with the regular variant that doesn't modify the original input:
#' y <- vec_assign(x, 3, 500)
#' y
#' x
#'
#'
#' # Slicing objects of higher dimension:
#' vec_slice(mtcars, 1:3)
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
#' vec_ptype(x)
#'
#' # Compare to `[<-`:
#' x[2] <- 20.0
#' vec_ptype(x)
#'
#'
#' # Note that the types must be coercible for the cast to happen.
#' # For instance, you can cast a character vector to an integer:
#' vec_cast("1", integer())
#'
#' # But these types are not coercible:
#' try(vec_ptype2("1", integer()))
#'
#' # Hence you cannot assign character values to an integer or double
#' # vector:
#' try(vec_slice(x, 2) <- "20")
vec_slice <- function(x, i) {
  .Call(vctrs_slice, x, i)
}

# Called when `x` has dimensions
vec_slice_fallback <- function(x, i) {
  out <- unclass(vec_proxy(x))
  vec_assert(out)

  d <- vec_dim_n(out)
  if (d == 2) {
    out <- out[i, , drop = FALSE]
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    out <- eval_bare(expr(out[i, !!!miss_args, drop = FALSE]))
  }

  vec_restore(out, x)
}

#' @rdname vec_slice
#' @export
`vec_slice<-` <- function(x, i, value) {
  .Call(vctrs_assign, x, i, value)
}
#' @rdname vec_slice
#' @export
vec_assign <- function(x, i, value) {
  .Call(vctrs_assign, x, i, value)
}
vec_assign_fallback <- function(x, i, value) {
  # Work around bug in base `[<-`
  existing <- !is.na(i)
  i <- vec_slice(i, existing)
  value <- vec_slice(value, existing)

  d <- vec_dim_n(x)
  miss_args <- rep(list(missing_arg()), d - 1)
  eval_bare(expr(x[i, !!!miss_args] <- value))
  x
}

#' Create an index
#'
#' This provides a means of standardizing common indexing methods such as
#' integer, character or logical indexing. The input is a vector of one
#' of these three types, and the output is always an integer vector that can
#' be used for subsetting.
#'
#' @inheritParams vec_slice
#' @param n A single integer representing the total size of the
#'   object that `i` is meant to index into.
#' @param names If `i` is a character vector, `names` should be a character
#'   vector that `i` will be matched against to construct the index. Otherwise,
#'   not used. The default value of `NULL` will result in an error
#'   if `i` is a character vector.
#'
#' @return
#' An integer vector that can be used as an index in a subsetting operation.
#'
#' @examples
#' x <- array(1:6, c(2, 3))
#' dimnames(x) <- list(c("r1", "r2"), c("c1", "c2", "c3"))
#'
#' # The most common use case validates row indices
#' vec_as_index(1, vec_size(x))
#'
#' # Negative indices can be used to index from the back
#' vec_as_index(-1, vec_size(x))
#'
#' # Character vectors can be used if `names` are provided
#' vec_as_index("r2", vec_size(x), rownames(x))
#'
#' # You can also construct an index for dimensions other than the first
#' vec_as_index(c("c2", "c1"), ncol(x), colnames(x))
#'
#' @keywords internal
#' @export
vec_as_index <- function(i, n, names = NULL) {
  vec_assert(n, integer(), 1L)
  .Call(vctrs_as_index, i, n, names)
}

vec_index <- function(x, i, ...) {
  i <- maybe_missing(i, TRUE)

  if (!dots_n(...)) {
    return(vec_slice(x, i))
  }

  out <- unclass(vec_proxy(x))
  vec_assert(out)

  i <- vec_as_index(i, vec_size(x), vec_names(x))
  vec_restore(out[i, ..., drop = FALSE], x, n = length(i))
}

#' Initialize a vector
#'
#' @param x Template of vector to initialize.
#' @param n Desired size of result.
#' @export
#' @examples
#' vec_init(1:10, 3)
#' vec_init(Sys.Date(), 5)
#' vec_init(mtcars, 2)
vec_init <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}
