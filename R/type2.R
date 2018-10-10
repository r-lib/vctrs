#' Find the common type for a pair of vector types
#'
#' `vec_type2()` finds the common type for a pair of vectors, or dies trying.
#' It forms the foundation of the vctrs type system, along with [vec_cast()].
#' This powers type coercion but should not usually be called directly;
#' instead call [vec_type_common()].
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
#' `vec_type2()` dispatches on both arguments. This is implemented by having
#' methods of `vec_type2()`, e.g. `vec_type2.integer()` also be S3 generics,
#' which call e.g. `vec_type2.integer.double()`. `vec_type2.x.y()` must
#' return the same value as `vec_type2.y.x()`; this is currently not enforced,
#' but should be tested.
#'
#' Whenever you implemenet a `vec_type2.new_class()` generic/method,
#' make sure to always provide `vec_type2.new_class.default()` (
#' which should call [stop_incompatible_cast()]) and
#' `vec_type2.new_class.vctrs_unspecified()` (which should return `x`).
#'
#' See `vignette("s3-vector")` for full details.
#' @keywords internal
#' @param x,y Either vector types; i.e.
#' @export
vec_type2 <- function(x, y) {
  UseMethod("vec_type2")
}

#' @export
vec_type2.default <- function(x, y) {
  if (identical(attributes(x), attributes(y)))
    return(x)

  stop_incompatible_type(x, y)
}

# Numeric-ish ----------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.logical
#' @method vec_type2 logical
#' @export
vec_type2.logical <- function(x, y) UseMethod("vec_type2.logical", y)
#' @rdname vec_type2
#' @export vec_type2.integer
#' @method vec_type2 integer
#' @export
vec_type2.integer <- function(x, y) UseMethod("vec_type2.integer", y)
#' @rdname vec_type2
#' @export vec_type2.double
#' @method vec_type2 double
#' @export
vec_type2.double  <- function(x, y) UseMethod("vec_type2.double", y)

#' @method vec_type2.logical logical
#' @export
vec_type2.logical.logical <- function(x, y) shape_match(logical(), x, y)

#' @export
#' @method vec_type2.integer integer
vec_type2.integer.integer <- function(x, y) shape_match(integer(), x, y)
#' @export
#' @method vec_type2.logical integer
vec_type2.logical.integer <- function(x, y) shape_match(integer(), x, y)
#' @export
#' @method vec_type2.integer logical
vec_type2.integer.logical <- function(x, y) shape_match(integer(), x, y)

#' @export
#' @method vec_type2.double double
vec_type2.double.double   <- function(x, y) shape_match(double(), x, y)
#' @export
#' @method vec_type2.logical double
vec_type2.logical.double  <- function(x, y) shape_match(double(), x, y)
#' @export
#' @method vec_type2.double logical
vec_type2.double.logical  <- function(x, y) shape_match(double(), x, y)
#' @export
#' @method vec_type2.integer double
vec_type2.integer.double  <- function(x, y) shape_match(double(), x, y)
#' @export
#' @method vec_type2.double integer
vec_type2.double.integer  <- function(x, y) shape_match(double(), x, y)

#' @method vec_type2.logical default
#' @export
vec_type2.logical.default <- function(x, y) stop_incompatible_type(x, y)
#' @method vec_type2.integer default
#' @export
vec_type2.integer.default <- function(x, y) stop_incompatible_type(x, y)
#' @method vec_type2.double default
#' @export
vec_type2.double.default  <- function(x, y) stop_incompatible_type(x, y)

# Character ---------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.character
#' @method vec_type2 character
#' @export
vec_type2.character <- function(x, y) UseMethod("vec_type2.character", y)
#' @method vec_type2.character character
#' @export
vec_type2.character.character <- function(x, y) shape_match(character(), x, y)
#' @method vec_type2.character default
#' @export
vec_type2.character.default <- function(x, y) stop_incompatible_type(x, y)

# Lists -------------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.list
#' @method vec_type2 list
#' @export
vec_type2.list    <- function(x, y) UseMethod("vec_type2.list", y)
#' @method vec_type2.list list
#' @export
vec_type2.list.list <- function(x, y) shape_match(list(), x, y)
#' @method vec_type2.list default
#' @export
vec_type2.list.default  <- function(x, y) stop_incompatible_type(x, y)
