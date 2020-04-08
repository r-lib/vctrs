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
#' return the same value as `vec_ptype2.y.x()`; this is not enforced
#' for reasons of efficiency, but should be tested.
#'
#' `vec_ptype2()` are not inherited, classes must explicitly implement
#' the methods. There are two reasons for this:
#'
#' - The coercion hierarchy is often different from a class hierarchy.
#'   For instance the richer type between a tibble and a data frame is
#'   a tibble. Grouped data frames inherit from tibble, and so would by
#'   default inherit from tibble's `vec_ptype2()` method if inheritance
#'   was allowed. The method would then indicate that the richer type
#'   between a grouped data frame and a data frame is a tibble, which
#'   is wrong.
#'
#' - `vec_ptype2()` should be symmetric, i.e. it should return the same
#'   type no matter the order of the inputs. With inheritance, this
#'   isn't the case when two classes have a common parent class. For
#'   instance the richer type between a tsibble and a tibble is
#'   tsibble. Similarly, grouped data frames are a richer type than
#'   tibble. Both of these classess have tibble as common parent
#'   class. With inheritance, `vec_ptype2(tsibble, gdf)` would return a
#'   tsibble via the tsibble-tibble method. `vec_ptype2(gdf, tsibble)`
#'   would return a grouped data frame via the gdf-tibble method.
#'
#' Because of the way double dispatch is implemented, `NextMethod()`
#' does not work inside `vec_ptype2()` methods.
#'
#' See `vignette("s3-vector")` for full details.
#' @keywords internal
#' @inheritParams ellipsis::dots_empty
#' @param x,y Vector types.
#' @param x_arg,y_arg Argument names for `x` and `y`. These are used
#'   in error messages to inform the user about the locations of
#'   incompatible types (see [stop_incompatible_type()]).
#' @export
vec_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_ptype2, x, y, x_arg, y_arg))
  UseMethod("vec_ptype2")
}
vec_ptype2_dispatch_s3 <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2")
}
#' @rdname vec_ptype2
#' @export
vec_default_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (is_asis(x)) {
    return(vec_ptype2_asis_left(x, y, x_arg = x_arg, y_arg = y_arg))
  }
  if (is_asis(y)) {
    return(vec_ptype2_asis_right(x, y, x_arg = x_arg, y_arg = y_arg))
  }

  if (is_same_type(x, y)) {
    return(vec_ptype(x, x_arg = x_arg))
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
vec_is_coercible <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_is_coercible, x, y, x_arg, y_arg)
}

vec_is_subtype <- function(x, super, ..., x_arg = "", super_arg = "") {
  tryCatch(
    vctrs_error_incompatible_type = function(...) FALSE,
    {
      common <- vctrs::vec_ptype2(x, super, ..., x_arg = x_arg, y_arg = super_arg)
      vec_is(common, super)
    }
  )
}
