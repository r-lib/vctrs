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

#' Create an index vector or a position
#'
#' These helpers provide a means of standardizing common indexing
#' methods such as integer, character or logical indexing.
#'
#' * `vec_as_index()` accepts integer, character, or logical vectors
#'   of any sizes. The output is always an integer vector of the same
#'   size that can be used for subsetting. This is suitable for
#'   indexing with `[` or [vec_slice()].
#'
#' * `vec_as_position()` accepts a single number or string. It returns
#'   a single position as a integer vector of size 1. This is suitable
#'   for extracting with `[[`.
#'
#' @inheritParams vec_slice
#' @param n A single integer representing the total size of the
#'   object that `i` is meant to index into.
#' @param names If `i` is a character vector, `names` should be a character
#'   vector that `i` will be matched against to construct the index. Otherwise,
#'   not used. The default value of `NULL` will result in an error
#'   if `i` is a character vector.
#'
#' @return `vec_as_index()` returns an integer vector that can be used
#'   as an index in a subsetting operation. `vec_as_position()`
#'   returns an integer of size 1 that can be used a scalar index for
#'   extracting an element.
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
#' @rdname vec_as_index
#' @export
vec_as_position <- function(i, n, names = NULL) {
  if (is.object(i)) {
    if (vec_is_coercible(i, int())) {
      i <- vec_cast(i, int())
    } else if (vec_is_coercible(i, chr())) {
      i <- vec_cast(i, chr())
    }
  } else if (typeof(i) == "double") {
    i <- vec_coercible_cast(i, int())
  }

  type <- typeof(i)
  if (!type %in% c("integer", "character")) {
    stop_position_incompatible_type(i)
  }
  if (length(i) != 1L) {
    stop_position_incompatible_size(i)
  }
  if (type == "integer" && i < 1L) {
    stop_position_incompatible_sign(i)
  }

  vec_as_index(i, n, names = names)
}

stop_position_incompatible_type <- function(i) {
  # Should we derive from `stop_incompatible_type()` once we have
  # union types? The index is incompatible with `union<chr(), int()>`.
  abort("", "vctrs_error_index_position_bad_type", i = i)
}
stop_position_incompatible_size <- function(i) {
  abort("", "vctrs_error_index_position_bad_size", i = i)
}
stop_position_incompatible_sign <- function(i) {
  abort("", "vctrs_error_index_position_bad_sign", i = i)
}

#' @export
conditionMessage.vctrs_error_index_position_bad_type <- function(c) {
  if (!nzchar(c$message)) {
    type <- vec_ptype_full(c$i)
    c$message <- glue_lines(
      "Must extract with a single number or a name.",
      "`i` has the wrong type `{type}`."
    )
  }
  NextMethod()
}
#' @export
conditionMessage.vctrs_error_index_position_bad_size <- function(c) {
  if (!nzchar(c$message)) {
    size <- length(c$i)
    c$message <- glue_lines(
      "Must extract with a single number or a name.",
      "`i` has the wrong size `{size}`"
    )
  }
  NextMethod()
}
#' @export
conditionMessage.vctrs_error_index_position_bad_sign <- function(c) {
  if (!nzchar(c$message)) {
    c$message <- glue_lines(
      "Must extract with a positive number.",
      if (c$i == 0L) {
        "`i` can't be zero."
      } else {
        "`i` has the wrong sign: {c$i}."
      }
    )
  }
  NextMethod()
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

# Used internally by `vec_rbind()`, but exported for testing
vec_split_along <- function(x) {
  .Call(vctrs_split_along, x)
}

# Exposed for testing (`start` is 0-based)
vec_slice_seq <- function(x, start, size, increasing = TRUE) {
  .Call(vctrs_slice_seq, x, start, size, increasing)
}
