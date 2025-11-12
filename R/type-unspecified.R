#' A 1d vector of unspecified type
#'
#' @description
#' This is the underlying type used to represent logical vectors that only
#' contain `NA`. These require special handling because we want to allow logical
#' `NA` to specify missingness that can be cast to any other type.
#'
#' [vec_ptype()] and [vec_ptype2()] convert a logical vector of `NA` into an
#' empty `<unspecified>` type. This type can combine with any other type.
#'
#' [vec_ptype_common()] uses both [vec_ptype()] and [vec_ptype2()] to compute
#' the common type, but then returns a _finalised_ type using
#' [vec_ptype_finalise()]. The purpose of `vec_ptype_finalise()` is to turn any
#' remaining `<unspecified>` types back into `<logical>`, which is the more
#' useful type for callers of `vec_ptype_common()`.
#'
#' `vec_ptype_finalise()` is an S3 generic, but it is extremely rare to need to
#' write an S3 method for this. Data frames (and data frame subclasses) are
#' already recursively finalised by the default method. The only time you may
#' need to write an S3 method for `vec_ptype_finalise()` is if your class
#' _wraps_ an arbitrary vector that has the potential to be a logical vector
#' containing only `NA`s. See `ivs::iv()` for an example of this, which wraps
#' arbitrary `start` and `end` vectors of the same type into a single interval
#' vector class.
#'
#' @keywords internal
#' @name vctrs-unspecified
#'
#' @examples
#' # Returns `unspecified()`
#' vec_ptype(NA)
#' vec_ptype(c(NA, NA))
#'
#' # We've chosen to make this return `logical()`, but this is admittedly
#' # ambiguous, as it could be seen as "an empty vector of `NA`s" that could
#' # also be treated as unspecified.
#' vec_ptype(logical())
#'
#' # These return `unspecified()`
#' vec_ptype2(NA, NA)
#' vec_ptype2(NA, NULL)
#' vec_ptype2(NULL, NA)
#'
#' # An unspecified vector can combine with any other type
#' vec_ptype2(NA, "x")
#' vec_ptype2("x", NA)
#'
#' # Same as using `unspecified()` directly
#' vec_ptype2(unspecified(1), "x")
#' vec_ptype2("x", unspecified(1))
#'
#' # Finalising a ptype turns unspecified back to logical
#' vec_ptype(NA)
#' vec_ptype_finalise(vec_ptype(NA))
#'
#' # This works recursively over data frames
#' df <- data_frame(x = NA, y = data_frame(z = NA))
#' vec_ptype_show(vec_ptype(df))
#' vec_ptype_show(vec_ptype_finalise(vec_ptype(df)))
#'
#' # `vec_ptype_common()` finalises automatically rather than returning an
#' # unspecified type
#' vec_ptype_common(NA)
#' vec_ptype_common(NA, NA)
#' vec_ptype_show(vec_ptype_common(df))
NULL

#' @param n Length of vector
#'
#' @rdname vctrs-unspecified
#' @export
unspecified <- function(n = 0) {
  .Call(vctrs_unspecified, n)
}

#' @export
`[.vctrs_unspecified` <- function(x, i, ...) {
  unspecified(length(NextMethod()))
}

#' @export
print.vctrs_unspecified <- function(x, ...) {
  cat("<unspecified> [", length(x), "]\n", sep = "")
}

#' @export
vec_ptype_abbr.vctrs_unspecified <- function(x, ...) {
  "???"
}

is_unspecified <- function(x) {
  .Call(vctrs_is_unspecified, x)
}

ununspecify <- function(x) {
  if (is_unspecified(x)) {
    new_logical(length(x))
  } else {
    x
  }
}

#' @inheritParams rlang::args_dots_empty
#'
#' @param x A `ptype` to finalize, typically a result of [vec_ptype()] or
#'   [vec_ptype2()].
#'
#' @rdname vctrs-unspecified
#' @export
vec_ptype_finalise <- function(x, ...) {
  check_dots_empty0(...)
  return(.Call(vctrs_ptype_finalise, x))
  UseMethod("vec_ptype_finalise")
}

vec_ptype_finalise_dispatch <- function(x, ...) {
  UseMethod("vec_ptype_finalise")
}

#' @export
vec_ptype_finalise.default <- function(x, ...) {
  x
}
