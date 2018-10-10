#' Cast a vector to specified type
#'
#' `vec_cast()` provides general coercions from one type of vector to another,
#' and along with [vec_type2()] forms the foundation of the vctrs type system.
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
#' lossy, a warning message will be shown whenever an actual loss occurs
#' (which may only be for some elements of a vector).
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
#' Note that `vec_cast()` dispatch on its second argument, so that the name
#' of the final method uses the same convention as `as.xyz()` methods, i.e.
#' `vec_cast.integer.double()` casts double to integers, in the same way
#' that `as.integer.double()` would.
#'
#' Whenever you implemenet a `vec_cast.new_class()` generic/method,
#' make sure to always provide `vec_cast.new_class.default()` (
#' which should call [stop_incompatible_cast()]) and
#' `vec_cast.new_class.vctrs_unspecified()` (which should call
#' [vec_unspecified_cast()]).
#'
#' See `vignette("s3-vector")` for full details.
#' @section Restoring attributes:
#'
#' A restore is a specialised type of cast, primarily used in conjunction
#' with `NextMethod()` or a C-level function that works on the underlying
#' data structure. A `vec_restore()` method can assume that `x` has the
#' correct type (although the length may be different) but all attributes
#' have been lost and need to be restored. In other words,
#' `vec_restore(vec_data(x), x)` should yield `x`.
#'
#' To understand the difference between `vec_cast()` and `vec_restore()`
#' think about factors: it doesn't make sense to cast an integer to a factor,
#' but if `NextMethod()` or other low-level function has stripped attributes,
#' you still need to be able to restore them.
#'
#' The default method copies across all attributes so you only need to
#' provide your own method if your attributes require special care
#' (i.e. they are dependent on the data in somew way). When implementing
#' your own method, bear in mind that many R users add attributes to track
#' additional metadat that is important to them, so you should preserve any
#' attributes that don't require special handling for your class.
#'
#' @param x,... Vectors to cast.
#' @param to,.to Type to cast to. If `NULL`, `x` will be returned as is.
#' @return A vector the same length as `x` with the same type as `to`,
#'   or an error if the cast is not possible. A warning is generated if
#'   information is lost when casting between compatible types (i.e. when
#'   there is no 1-to-1 mapping for a specific value).
#' @export
#' @keywords internal
#' @examples
#' # x is a double, but no information is lost
#' vec_cast(1, integer())
#'
#' # Information is lost so a warning is generated
#' \dontrun{
#' vec_cast(1.5, integer())
#' }
#'
#' # No sensible coercion is possible so an error is generated
#' \dontrun{
#' vec_cast(1.5, factor("a"))
#' }
#'
#' # Cast to common type
#' vec_cast_common(factor("a"), factor(c("a", "b")))
#' vec_cast_common(factor("a"), Sys.Date(), .to = list())
vec_cast <- function(x, to) {
  if (is.null(x) || is.null(to)) {
    return(x)
  }
  UseMethod("vec_cast", to)
}

#' @export
#' @rdname vec_cast
vec_cast_common <- function(..., .to = NULL) {
  args <- list2(...)
  type <- vec_type_common(!!!args, .ptype = .to)
  map(args, vec_cast, to = type)
}

#' @export
vec_cast.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @rdname vec_cast
vec_restore <- function(x, to) {
  UseMethod("vec_restore", to)
}

#' @export
vec_restore.default <- function(x, to) {
  attributes(x) <- attributes(to)
  x
}

# Base vectors --------------------------------------------------------------

#' @export
#' @rdname vec_cast
#' @export vec_cast.logical
#' @method vec_cast logical
vec_cast.logical <- function(x, to) UseMethod("vec_cast.logical")
#' @export
#' @method vec_cast.logical logical
vec_cast.logical.logical <- function(x, to) {
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.logical integer
vec_cast.logical.integer <- function(x, to) {
  report_lossy_cast(x, to, !x %in% c(0L, 1L))
  x <- vec_coerce_bare(x, "logical")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.logical double
vec_cast.logical.double <- function(x, to) {
  report_lossy_cast(x, to, !x %in% c(0, 1))
  x <- vec_coerce_bare(x, "logical")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.logical character
vec_cast.logical.character <- function(x, to) {
  report_lossy_cast(x, to, !toupper(x) %in% c("T", "F", "TRUE", "FALSE"))
  x <- vec_coerce_bare(x, "logical")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.logical list
vec_cast.logical.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.logical default
vec_cast.logical.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.integer
#' @method vec_cast integer
vec_cast.integer <- function(x, to) {
  UseMethod("vec_cast.integer")
}
#' @export
#' @method vec_cast.integer logical
vec_cast.integer.logical <- function(x, to) {
  x <- vec_coerce_bare(x, "integer")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.integer integer
vec_cast.integer.integer <- function(x, to) {
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.integer double
vec_cast.integer.double <- function(x, to) {
  out <- suppressWarnings(vec_coerce_bare(x, "integer"))
  report_lossy_cast(x, to, (out != x) | xor(is.na(x), is.na(out)))

  shape_broadcast(out, to)
}
#' @export
#' @method vec_cast.integer character
vec_cast.integer.character <- vec_cast.integer.double
#' @export
#' @method vec_cast.integer list
vec_cast.integer.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.integer default
vec_cast.integer.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.double
#' @method vec_cast double
vec_cast.double <- function(x, to) {
  UseMethod("vec_cast.double")
}
#' @export
#' @method vec_cast.double logical
vec_cast.double.logical <- function(x, to) {
  x <- vec_coerce_bare(x, "double")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.double integer
vec_cast.double.integer <- vec_cast.double.logical
#' @export
#' @method vec_cast.double character
vec_cast.double.character <- function(x, to) {
  out <- suppressWarnings(vec_coerce_bare(x, "double"))
  report_lossy_cast(x, to, (out != x) | xor(is.na(x), is.na(out)))

  shape_broadcast(out, to)
}
#' @export
#' @method vec_cast.double double
vec_cast.double.double <- function(x, to) {
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.double list
vec_cast.double.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.double default
vec_cast.double.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.character
#' @method vec_cast character
vec_cast.character <- function(x, to) {
  UseMethod("vec_cast.character")
}
#' @export
#' @method vec_cast.character logical
vec_cast.character.logical <- function(x, to) {
  x <- vec_coerce_bare(x, "character")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.character integer
vec_cast.character.integer <- vec_cast.character.logical
#' @export
#' @method vec_cast.character double
vec_cast.character.double <- vec_cast.character.logical
#' @export
#' @method vec_cast.character character
vec_cast.character.character <- function(x, to) {
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.character difftime
vec_cast.character.difftime <- function(x, to) {
  x <- paste(x, units(x))
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.character list
vec_cast.character.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.character default
vec_cast.character.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.list
#' @method vec_cast list
#' @export
vec_cast.list <- function(x, to) {
  UseMethod("vec_cast.list")
}
#' @export
#' @method vec_cast.list list
vec_cast.list.list <- function(x, to) {
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.list default
vec_cast.list.default <- function(x, to) {
  out <- lapply(seq_along(x), function(i) x[[i]])

  if (!is.object(to)) {
    out <- shape_broadcast(out, to)
  }

  out
}

# Helpers -----------------------------------------------------------------

# Used primarily to make base coercions a little stricter
report_lossy_cast <- function(x, y, lossy, details = NULL) {
  if (all(lossy)) {
    stop_incompatible_cast(
      x, y,
      details = "All elements of vectorised cast failed"
    )
  }
  if (any(lossy)) {
    warn_lossy_cast(x, y, locations = which(lossy), details = details)
  }
}

