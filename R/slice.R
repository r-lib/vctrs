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
#'   of any size. The output is always an integer vector that is
#'   suitable for subsetting with `[` or [vec_slice()]. It might be a
#'   different size than the input because negative selections are
#'   transformed to positive ones and logical vectors are transformed
#'   to a vector of indices for the `TRUE` positions.
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
#' @param arg The argument name to be displayed in error messages when
#'   `vec_as_index()` and `vec_as_position()` are used to check the
#'   type of a function input.
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
vec_as_index <- function(i, n, names = NULL, ..., arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  vec_assert(n, integer(), 1L)
  i <- vec_coerce_index(i, arg = arg)
  .Call(vctrs_as_index, i, n, names)
}
vec_coerce_index <- function(i, ..., arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  maybe_get(vec_maybe_coerce_index(i, arg = arg))
}
vec_maybe_coerce_index <- function(i, arg) {
  if (!vec_is(i)) {
    return(maybe(error = new_error_index_bad_type(i, .arg = arg)))
  }

  nms <- names(i)

  if (is.object(i)) {
    if (vec_is_subtype(i, int())) {
      i <- vec_cast(i, int())
    } else if (vec_is_subtype(i, chr())) {
      i <- vec_cast(i, chr())
    } else {
      return(maybe(error = new_error_index_bad_type(i, .arg = arg)))
    }
  } else if (is_double(i)) {
    maybe <- tryCatch(
    {
      i <- vec_coercible_cast(i, int(), x_arg = arg, to_arg = "")
      names(i) <- nms
      maybe(i)
    },
    vctrs_error_cast_lossy = function(err) {
      maybe(error = new_error_index_bad_type(
        i = i,
        parent = err,
        .bullets = cnd_bullets_index_lossy_cast
      ))
    })
    return(maybe)
  }

  if (!typeof(i) %in% c("integer", "character", "logical")) {
    return(maybe(error = new_error_index_bad_type(i, .arg = arg)))
  }

  # FIXME: Work around lack of name restoration in `vec_cast()`
  names(i) <- nms

  maybe(i)
}

#' @rdname vec_as_index
#' @export
vec_as_position <- function(i, n, names = NULL, ..., arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  maybe_get(vec_maybe_as_position(i, n = n, names = names, arg = arg))
}
vec_coerce_position <- function(i, ..., arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  maybe_get(vec_maybe_coerce_position(i, arg))
}

vec_maybe_coerce_position <- function(i, arg) {
  if (is.object(i) && vec_is(i) && vec_is_subtype(i, lgl())) {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      .arg = arg,
      .bullets = cnd_bullets_position_bad_base_type
    )))
  }

  maybe <- vec_maybe_coerce_index(i, arg)

  # Return a subclass of index error
  if (!is_null(maybe$error)) {
    parent <- maybe$error$parent
    if (inherits(parent, "vctrs_error_cast_lossy")) {
      bullets <- cnd_bullets_index_lossy_cast
    } else {
      bullets <- cnd_bullets_position_bad_base_type
    }

    maybe$error <- new_error_position_bad_type(
      i = maybe$error$i,
      .arg = arg,
      .bullets = bullets,
      parent = maybe$error$parent
    )

    return(maybe)
  }

  i <- maybe$value

  if (typeof(i) == "logical") {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      .arg = arg,
      .bullets = cnd_bullets_position_bad_base_type
    )))
  }

  maybe
}
vec_maybe_as_position <- function(i, n, names = NULL, arg = "i") {
  maybe <- vec_maybe_coerce_position(i, arg = arg)

  if (!is_null(maybe$error)) {
    return(maybe)
  }

  # Positions must be size 1, can't be NA, and must be positive
  i <- maybe$value

  if (length(i) != 1L) {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      .arg = arg,
      .bullets = cnd_bullets_position_need_scalar
    )))
  }

  if (is.na(i)) {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      .arg = arg,
      .bullets = cnd_bullets_position_need_present
    )))
  }

  if (typeof(i) == "integer" && i < 1L) {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      .arg = arg,
      .bullets = cnd_bullets_position_need_positive
    )))
  }

  # FIXME: Use maybe approach in internal implementation?
  tryCatch(
    maybe(.Call(vctrs_as_index, i, n, names)),
    # FIXME: All index error should inherit from "vctrs_error_index"
    vctrs_error_index_bad_type = function(err) {
      maybe(error = new_error_position_bad_type(i, parent = err, .arg = arg))
    }
  )
}

new_index_error <- function(.subclass = NULL, i, ..., .arg = "i") {
  error_cnd(
    .subclass = c(.subclass, "vctrs_error_index"),
    i = i,
    .arg = .arg,
    ...
  )
}
new_error_index_bad_type <- function(i, ..., .arg = "i", .subclass = NULL) {
  new_index_error(
    .subclass = c(.subclass, "vctrs_error_index_bad_type"),
    i = i,
    .arg = .arg,
    ...
  )
}
new_error_position_bad_type <- function(i, ..., .arg = "i", .subclass = NULL) {
  new_error_index_bad_type(
    .subclass = c(.subclass, "vctrs_error_position_bad_type"),
    i = i,
    .arg = .arg,
    ...
  )
}

#' @export
cnd_issue.vctrs_error_index_bad_type <- function(cnd) {
  "Must subset with positions or names."
}
#' @export
cnd_bullets.vctrs_error_index_bad_type <- function(cnd) {
  arg <- cnd$.arg %||% "i"
  type <- obj_type(cnd$i)

  c(
    x = glue::glue("`{arg}` has the wrong type `{type}`."),
    i = "Positions and names must be integer, logical, or character."
  )
}
cnd_bullets_index_lossy_cast <- function(cnd, ...) {
  c(x = cnd_issue(cnd$parent))
}

#' @export
cnd_issue.vctrs_error_position_bad_type <- function(cnd) {
  "Must extract with a single position or name."
}

cnd_bullets_position_bad_base_type <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  type <- obj_type(cnd$i)
  c(
    x = glue::glue("`{arg}` has the wrong type `{type}`."),
    i = "Positions and names must be integer or character."
  )
}
cnd_bullets_position_need_scalar <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  size <- length(cnd$i)
  c(
    x = glue::glue("`{arg}` has the wrong size {size}."),
    i = "Positions and names must be size 1."
  )
}
cnd_bullets_position_need_present <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  c(
    x = glue::glue("`{arg}` can't be `NA`."),
    i = "Positions and names can't be missing."
  )
}
cnd_bullets_position_need_positive <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  i <- cnd$i
  c(
    x =
      if (i == 0L) {
        glue::glue("`{arg}` can't be zero.")
      } else {
        glue::glue("`{arg}` (with value {i}) has the wrong sign.")
      },
    i = "Positions must be positive integers."
  )
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
