#' Get or set observations in a vector
#'
#' This provides a common interface to extracting and modifying observations
#' for all vector types, regardless of dimensionality. They are analogs to `[`
#' and `[<-` that match [vec_size()] instead of `length()`.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A vector
#'
#' @param i An integer, character or logical vector specifying the locations or
#'   names of the observations to get/set. Specify `TRUE` to index all elements
#'   (as in `x[]`), or `NULL`, `FALSE` or `integer()` to index none (as in
#'   `x[NULL]`).
#'
#' @param value A vector of replacement values
#'
#'   `value` is cast to the type of `x`.
#'
#'   If `slice_value = FALSE`, `value` must be size 1 or the same size as `i`
#'   after `i` has been converted to a positive integer location vector with
#'   [vec_as_location()] (which may not be the same size as `i` originally).
#'
#'   If `slice_value = TRUE`, `value` must be size 1 or the same size as `x`.
#'
#' @param slice_value A boolean. If `TRUE`, the assignment proceeds as if you
#'   had provided `vec_slice(x, i) <- vec_slice(value, i)`, but is optimized to
#'   avoid materializing the slice of `value`.
#'
#' @param x_arg,value_arg Argument names for `x` and `value`. These are used
#'   in error messages to inform the user about the locations of
#'   incompatible types and sizes (see [stop_incompatible_type()] and
#'   [stop_incompatible_size()]).
#'
#' @return A vector of the same type as `x`.
#'
#' @section Genericity:
#'
#' Support for S3 objects depends on whether the object implements a
#' [vec_proxy()] method.
#'
#' * When a `vec_proxy()` method exists, the proxy is sliced or assigned to and
#'   `vec_restore()` is called on the result.
#'
#' * Otherwise, `vec_slice()` falls back to the base generic `[` and
#'   `vec_slice<-()` falls back to the base generic `[<-`.
#'
#' When `vec_slice<-()` falls back to `[<-`, it is expected that the subclass's
#' `[<-` method can handle the following subset of cases that base R's `[<-`
#' can also handle:
#'
#' * An `i` vector of positive integer positions (notably excluding `NA`).
#'
#' * A `value` vector of length 1 or length `length(i)`. If length 1, it
#'   should be recycled by the `[<-` method to the length of `i`.
#'
#' If your `[<-` method eventually calls base R's native `[<-` code, then these
#' cases will be handled for you.
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
#' @section Dependencies:
#'
#' ## vctrs dependencies
#'
#' - [vec_proxy()]
#' - [vec_restore()]
#'
#' ## base dependencies
#'
#' - \code{base::`[`}
#' - \code{base::`[<-`}
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
#' # For instance, you can cast a double vector of whole numbers to an
#' # integer vector:
#' vec_cast(1, integer())
#'
#' # But not fractional doubles:
#' try(vec_cast(1.5, integer()))
#'
#' # For this reason you can't assign fractional values in an integer
#' # vector:
#' x <- 1:3
#' try(vec_slice(x, 2) <- 1.5)
#'
#' # Slicing `value` -------------------------------------------------
#'
#' # Sometimes both `x` and `value` start from objects that are the same length,
#' # and you need to slice `value` by `i` before assigning it to `x`. This comes
#' # up when thinking about how `base::ifelse()` and `dplyr::case_when()` work.
#' condition <- c(TRUE, FALSE, TRUE, FALSE)
#' yes <- 1:4
#' no <- 5:8
#'
#' # Create an output container and fill it
#' out <- vec_init(integer(), 4)
#' out <- vec_assign(out, condition, vec_slice(yes, condition))
#' out <- vec_assign(out, !condition, vec_slice(no, !condition))
#' out
#'
#' # This is wasteful because you have to materialize the slices of `yes` and
#' # `no` before they can be assigned, and you also have to validate `condition`
#' # multiple times. Using `slice_value` internally performs
#' # `vec_slice(yes, condition)` and `vec_slice(no, !condition)` for you,
#' # but does so in a way that avoids the materialization.
#' out <- vec_init(integer(), 4)
#' out <- vec_assign(out, condition, yes, slice_value = TRUE)
#' out <- vec_assign(out, !condition, no, slice_value = TRUE)
#' out
vec_slice <- function(x, i, ..., error_call = current_env()) {
  check_dots_empty0(...)
  .Call(ffi_slice, x, i, environment())
}

# Called when `x` has dimensions
vec_slice_fallback <- function(x, i) {
  out <- unclass(vec_proxy(x))
  obj_check_vector(out)

  d <- vec_dim_n(out)
  if (d == 2) {
    out <- out[i, , drop = FALSE]
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    out <- eval_bare(expr(out[i, !!!miss_args, drop = FALSE]))
  }

  vec_restore(out, x)
}

vec_slice_fallback_integer64 <- function(x, i) {
  d <- vec_dim_n(x)

  if (d == 2) {
    out <- x[i, , drop = FALSE]
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    out <- eval_bare(expr(x[i, !!!miss_args, drop = FALSE]))
  }

  is_na <- is.na(i)

  if (!any(is_na)) {
    return(out)
  }

  if (d == 2) {
    out[is_na,] <- bit64::NA_integer64_
  } else {
    eval_bare(expr(out[is_na, !!!miss_args] <- bit64::NA_integer64_))
  }

  out
}

# bit64::integer64() objects do not have support for `NA_integer_`
# slicing. This manually replaces the garbage values that are created
# any time a slice with `NA_integer_` is made.
vec_slice_dispatch_integer64 <- function(x, i) {
  out <- x[i]

  is_na <- is.na(i)

  if (!any(is_na)) {
    return(out)
  }

  out[is_na] <- bit64::NA_integer64_

  out
}


#' @rdname vec_slice
#' @export
`vec_slice<-` <- function(x, i, value) {
  x_arg <- "" # Substitution is `*tmp*`
  delayedAssign("value_arg", as_label(substitute(value)))
  slice_value <- FALSE
  .Call(ffi_assign, x, i, value, slice_value, environment())
}
#' @rdname vec_slice
#' @export
vec_assign <- function(
  x,
  i,
  value,
  ...,
  slice_value = FALSE,
  x_arg = "",
  value_arg = ""
) {
  check_dots_empty0(...)
  .Call(ffi_assign, x, i, value, slice_value, environment())
}

# Invariants for `[<-` methods:
#
# - `i` will contain positive integers
# - `i` will not contain `NA` (a base `[<-` issue gets in the way if we allow this)
# - `value` will be size 1 or size `length(i)`
#
# Given these invariants, the base `[<-` works the way we want it to in fallback
# methods, and we expect any subclasses to also uphold the same behavior as base
# `[<-` with these inputs. In other words, we don't expect subclasses to have
# vctrs subassign behavior, but we do expect them to match a subset of base R
# subassign behavior.
vec_assign_fallback <- function(x, i, value, slice_value, index_style) {
  if (index_style == "condition") {
    # Convert logical condition `i` to integer location `i`. Must use
    # `vec_as_location()` rather than `which()` because we want `NA` values to
    # propagate, which is consistent with how `value`'s size is checked
    # internally (i.e. when `slice_value = FALSE`, `value` was checked to have
    # size equal to the number of `TRUE` or `NA` values in `i`). Propagated
    # `NA`s are later dropped before calling `[<-` to work around a base `[<-`
    # issue, but we need them to slice both `i` and `value` consistently.
    i <- vec_as_location(i, n = vec_size(x), missing = "propagate")
  }

  if (slice_value && vec_size(value) != 1L) {
    # `value` has same size as `x` rather than same size as `i`.
    # We need to pre-slice it down to same size as `i` to match what `[<-` expects.
    # Effectively we are preemptively doing the RHS of this:
    # vec_slice(x, i) <- vec_slice(value, i)
    value <- vec_slice(value, i)
  }

  # Work around issue in base `[<-` that errors on `NA_integer_` in subassign
  # indices
  existing <- !is.na(i)
  i <- vec_slice(i, existing)
  if (vec_size(value) != 1L) {
    value <- vec_slice(value, existing)
  }

  d <- vec_dim_n(x)
  miss_args <- rep(list(missing_arg()), d - 1)
  eval_bare(expr(x[i, !!!miss_args] <- value))
  x
}

# `start` is 0-based
vec_assign_seq <- function(
  x,
  value,
  start,
  size,
  increasing = TRUE,
  slice_value = FALSE
) {
  .Call(ffi_assign_seq, x, value, start, size, increasing, slice_value)
}

#' @param assign_names A boolean. If `TRUE`, will assign names from `value`
#'   onto `x` as well.
#'
#' @noRd
vec_assign_params <- function(
  x,
  i,
  value,
  ...,
  assign_names = FALSE,
  slice_value = FALSE,
  x_arg = "",
  value_arg = ""
) {
  check_dots_empty0(...)
  .Call(
    ffi_assign_params,
    x,
    i,
    value,
    assign_names,
    slice_value,
    environment()
  )
}

vec_remove <- function(x, i) {
  vec_slice(x, -vec_as_location(i, length(x), names(x)))
}

vec_index <- function(x, i, ...) {
  i <- maybe_missing(i, TRUE)
  out <- vec_slice(x, i)

  if (!dots_n(...)) {
    return(out)
  }

  # Need to unclass to avoid infinite recursion through `[`
  proxy <- vec_data(out)
  out <- proxy[, ..., drop = FALSE]

  vec_restore(out, x)
}

#' Initialize a vector
#'
#' @param x Template of vector to initialize.
#' @param n Desired size of result.
#' @export
#' @section Dependencies:
#' * vec_slice()
#' @examples
#' vec_init(1:10, 3)
#' vec_init(Sys.Date(), 5)
#'
#' # The "missing" value for a data frame is a row that is entirely missing
#' vec_init(mtcars, 2)
#'
#' # The "missing" value for a list is `NULL`
#' vec_init(list(), 3)
vec_init <- function(x, n = 1L) {
  .Call(ffi_init, x, n, environment())
}

# Exposed for testing (`start` is 0-based)
vec_slice_seq <- function(x, start, size, increasing = TRUE) {
  .Call(ffi_slice_seq, x, start, size, increasing)
}

# Exposed for testing (`i` is 1-based)
vec_slice_rep <- function(x, i, n) {
  .Call(ffi_slice_rep, x, i, n)
}

# Forwards arguments to `base::rep()`
base_vec_rep <- function(x, ...) {
  i <- rep(seq_len(vec_size(x)), ...)
  vec_slice(x, i)
}

# Emulates `length<-`
vec_size_assign <- function(x, n) {
  x_size <- vec_size(x)

  if (n > x_size) {
    i <- seq_len(x_size)
    i <- c(i, vec_init(int(), n - x_size))
  } else {
    i <- seq_len(n)
  }

  vec_slice(x, i)
}
