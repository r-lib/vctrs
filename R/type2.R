#' Find the common type for a pair of vector types
#'
#' `vec_ptype2()` finds the common type for a pair of vectors, or dies trying.
#' It forms the foundation of the vctrs type system, along with [vec_cast()].
#' This powers type coercion but should not usually be called directly;
#' instead call [vec_ptype_common()].
#'
#' @section Coercion rules:
#' vctrs thinks of the vector types as forming a partially ordered set, or
#' poset. Then finding the common type from a set of types is a matter of
#' finding the least-upper-bound; if the least-upper-bound does not exist,
#' there is no common type. This is the case for many pairs of 1d vectors.
#'
#' The poset of the most important base vectors is shown below:
#' (where datetime stands for `POSIXt`, and date for `Date`)
#'
#' \figure{coerce.png}
#'
#' @section S3 dispatch:
#' `vec_ptype2()` dispatches on both arguments. This is implemented by having
#' methods of `vec_ptype2()`, e.g. `vec_ptype2.integer()` also be S3 generics,
#' which call e.g. `vec_ptype2.integer.double()`. `vec_ptype2.x.y()` must
#' return the same value as `vec_ptype2.y.x()`; this is currently not enforced,
#' but should be tested.
#'
#' Whenever you implement a `vec_ptype2.new_class()` generic/method,
#' make sure to always provide `vec_ptype2.new_class.default()`. It
#' should normally call `vec_default_ptype2()`.
#'
#' See `vignette("s3-vector")` for full details.
#' @keywords internal
#' @inheritParams ellipsis::dots_empty
#' @param x,y Vector types.
#' @param x_arg,y_arg Argument names for `x` and `y`. These are used
#'   in error messages to inform the user about the locations of
#'   incompatible types (see [stop_incompatible_type()]).
#' @export
vec_ptype2 <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_type2, x, y, x_arg, y_arg))
  UseMethod("vec_ptype2")
}
vec_ptype2_dispatch_s3 <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  UseMethod("vec_ptype2")
}
#' @export
vec_ptype2.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (has_same_type(x, y)) {
    return(x)
  }
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @rdname vec_ptype2
#' @export
vec_default_ptype2 <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is_unspecified(y)) {
    return(vec_ptype(x))
  }
  if (is_same_type(x, y)) {
    return(vec_ptype(x))
  }
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

vec_typeof2 <- function(x, y) {
  .Call(vctrs_typeof2, x, y)
}

vec_typeof2_s3 <- function(x, y) {
  .Call(vctrs_typeof2_s3, x, y)
}

# https://github.com/r-lib/vctrs/issues/571
vec_is_coercible <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_is_coercible, x, y, x_arg, y_arg)
}

vec_is_subtype <- function(x, super, ..., x_arg = "x", super_arg = "super") {
  tryCatch(
    vctrs_error_incompatible_type = function(...) FALSE,
    {
      common <- vctrs::vec_ptype2(x, super, ..., x_arg = x_arg, y_arg = super_arg)
      vec_is(common, super)
    }
  )
}
