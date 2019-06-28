#' Cast a vector to specified type
#'
#' `vec_cast()` provides general coercions from one type of vector to another,
#' and along with [vec_ptype2()] forms the foundation of the vctrs type system.
#' It should generally not be called by R users, but is important for R
#' developers. `vec_restore()` is designed specifically for casting a bare
#' vector to the original type; it's useful when relying `NextMethod()` for
#' the actual implementation. `vec_cast_common(...)` casts a collection to
#' vectors to the same type.
#'
#' @section Casting rules:
#' Casting is more flexible than coercion, and allows for the possibility of
#' information loss. This diagram summarises possible coercions. `vec_cast()`
#' from any type connected to another type, provided that the arrows are
#' followed in only one direction. For example you can cast from logical to
#' character, and list to time, but you can not cast from logical to datetime.
#'
#' \figure{cast.png}
#'
#' Most casts are not symmetric: you can cast all integers to doubles, but you
#' can only cast a subset of doubles back to integers. If a cast is potentially
#' lossy, an error will be shown whenever an actual loss occurs.
#'
#' The rules for coercing from a list are fairly strict: each component of the
#' list must be of length 1, and must be coercible to type `to`. This ensures
#' that a round-trip to and form list is possible, without opening the door
#' to very flexible list flattening (which should be the job of a more
#' specialised function).
#'
#' @section S3 dispatch:
#' `vec_cast()` dispatches on both arguments because casting depends on both
#' the type of `x` and of `to`. This is implemented by having methods of
#' `vec_cast()`, e.g. `vec_cast.integer()` also be S3 generics, which call
#' e.g. `vec_cast.integer.double()`.
#'
#' Note that `vec_cast()` dispatches on its second argument, so that the name
#' of the final method uses the same convention as `as.xyz()` methods, i.e.
#' `vec_cast.integer.double()` casts double to integers, in the same way
#' that `as.integer.double()` would.
#'
#' Whenever you implement a `vec_cast.new_class()` generic/method,
#' make sure to always provide `vec_cast.new_class.default()` and
#' call [vec_default_cast()] from that method.
#'
#' See `vignette("s3-vector")` for full details.
#'
#'
#' @section Restoring attributes:
#'
#' A restore is a specialised type of cast, primarily used in
#' conjunction with `NextMethod()` or a C-level function that works on
#' the underlying data structure. A `vec_restore()` method can make
#' the following assumptions about `x`:
#'
#' * It has the correct type.
#' * It has the correct names.
#' * It has the correct `dim` and `dimnames` attributes.
#' * It is unclassed. This way you can call vctrs generics with `x`
#'   without triggering an infinite loop of restoration.
#'
#' The length may be different (for example after [vec_slice()] has
#' been called), and all other attributes may have been lost. The
#' method should restore all attributes so that after restoration,
#' `vec_restore(vec_data(x), x)` yields `x`.
#'
#' To understand the difference between `vec_cast()` and `vec_restore()`
#' think about factors: it doesn't make sense to cast an integer to a factor,
#' but if `NextMethod()` or another low-level function has stripped attributes,
#' you still need to be able to restore them.
#'
#' The default method copies across all attributes so you only need to
#' provide your own method if your attributes require special care
#' (i.e. they are dependent on the data in some way). When implementing
#' your own method, bear in mind that many R users add attributes to track
#' additional metadata that is important to them, so you should preserve any
#' attributes that don't require special handling for your class.
#'
#' @param x Vectors to cast.
#' @param ... For `vec_cast_common()`, vectors to cast. For
#'   `vec_cast()` and `vec_restore()`, these dots are only for future
#'   extensions and should be empty.
#' @param to,.to Type to cast to. If `NULL`, `x` will be returned as is.
#' @param n \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'   The total size to restore to. This is currently passed by
#'   `vec_slice()` to solve edge cases arising in data frame
#'   restoration. In most cases you don't need this information and
#'   can safely ignore that argument. This parameter should be
#'   considered internal and experimental, it might change in the
#'   future.
#' @param x_arg,to_arg Argument names for `x` and `to`. These are used
#'   in error messages to inform the user about the locations of
#'   incompatible types (see [stop_incompatible_type()]).
#' @return A vector the same length as `x` with the same type as `to`,
#'   or an error if the cast is not possible. An error is generated if
#'   information is lost when casting between compatible types (i.e. when
#'   there is no 1-to-1 mapping for a specific value).
#' @export
#' @keywords internal
#' @examples
#' # x is a double, but no information is lost
#' vec_cast(1, integer())
#'
#' # When information is lost the cast fails
#' try(vec_cast(c(1, 1.5), integer()))
#' try(vec_cast(c(1, 2), logical()))
#'
#' # You can suppress this error and get the partial results
#' allow_lossy_cast(vec_cast(c(1, 1.5), integer()))
#' allow_lossy_cast(vec_cast(c(1, 2), logical()))
#'
#' # By default this suppress all lossy cast errors without
#' # distinction, but you can be specific about what cast is allowed
#' # by supplying prototypes
#' allow_lossy_cast(vec_cast(c(1, 1.5), integer()), to_ptype = integer())
#' try(allow_lossy_cast(vec_cast(c(1, 2), logical()), to_ptype = integer()))
#'
#' # No sensible coercion is possible so an error is generated
#' try(vec_cast(1.5, factor("a")))
#'
#' # Cast to common type
#' vec_cast_common(factor("a"), factor(c("a", "b")))
#' vec_cast_common(factor("a"), Sys.Date(), .to = list())
vec_cast <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_cast, x, to, x_arg, to_arg))
  UseMethod("vec_cast", to)
}
vec_cast_dispatch <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  UseMethod("vec_cast", to)
}

#' @export
#' @rdname vec_cast
vec_cast_common <- function(..., .to = NULL) {
  .External2(vctrs_cast_common, .to)
}

#' @export
vec_cast.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (has_same_type(x, to)) {
    return(x)
  }
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

# Cast `x` to `to` but only if they are coercible
vec_coercible_cast <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_coercible_cast, x, to, x_arg, to_arg)
}

#' Default cast method
#'
#' @description
#'
#' This function should typically be called from the default
#' [vec_cast()] method for your class, e.g. `vec_cast.myclass.default()`.
#' It does two things:
#'
#' * If `x` is an [unspecified] vector, it automatically casts it to
#'   `to` using [vec_init()].
#'
#' * Otherwise, an error is thrown with [stop_incompatible_cast()].
#'
#' @inheritParams vec_cast
#' @export
vec_default_cast <- function(x, to, x_arg = "x", to_arg = "to") {
  if (is_unspecified(x)) {
    vec_init(to, length(x))
  } else {
    stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }
}


#' @export
#' @rdname vec_cast
vec_restore <- function(x, to, ..., n = NULL) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_restore, x, to, n))
  UseMethod("vec_restore", to)
}
vec_restore_dispatch <- function(x, to, ..., n = NULL) {
  UseMethod("vec_restore", to)
}
#' @export
vec_restore.default <- function(x, to, ..., n = NULL) {
  .Call(vctrs_restore_default, x, to)
}
