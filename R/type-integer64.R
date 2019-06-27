
#' @export
vec_proxy_compare.integer64 <- function(x, ...) {
  bit64::rank.integer64(x)
}

# Print -------------------------------------------------------------------

#' 64 bit integers
#'
#' A `integer64` is a 64 bits integer vector, implemented in the `bit64` package.
#'
#' These functions help the `integer64` class from `bit64` in to
#' the vctrs type system by providing coercion functions
#' and casting functions.
#'
#' @keywords internal
#' @rdname int64
#' @export
vec_ptype_full.integer64 <- function(x, ...) {
  "integer64"
}

#' @rdname int64
#' @export
vec_ptype_abbr.integer64 <- function(x, ...) {
  "int64"
}


# Coerce ------------------------------------------------------------------

#' @export
#' @rdname int64
#' @export vec_ptype2.integer64
#' @method vec_ptype2 integer64
vec_ptype2.integer64 <- function(x, y, ...) {
  UseMethod("vec_ptype2.integer64", y)
}
#' @method vec_ptype2.integer64 default
#' @export
vec_ptype2.integer64.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.integer64 vctrs_unspecified
#' @export
vec_ptype2.integer64.vctrs_unspecified <- function(x, y, ...) bit64::integer64()
#' @method vec_ptype2.vctrs_unspecified integer64
#' @export
vec_ptype2.vctrs_unspecified.integer64 <- function(x, y, ...) bit64::integer64()

#' @method vec_ptype2.integer64 integer64
#' @export
vec_ptype2.integer64.integer64 <- function(x, y, ...) bit64::integer64()

#' @method vec_ptype2.integer64 integer
#' @export
vec_ptype2.integer64.integer <- function(x, y, ...) bit64::integer64()
#' @method vec_ptype2.integer integer64
#' @export
vec_ptype2.integer.integer64 <- function(x, y, ...) bit64::integer64()

#' @method vec_ptype2.integer64 logical
#' @export
vec_ptype2.integer64.logical <- function(x, y, ...) bit64::integer64()
#' @method vec_ptype2.logical integer64
#' @export
vec_ptype2.logical.integer64 <- function(x, y, ...) bit64::integer64()


# Cast --------------------------------------------------------------------

#' @export
#' @rdname int64
#' @export vec_cast.integer64
#' @method vec_cast integer64
vec_cast.integer64 <- function(x, to, ...) UseMethod("vec_cast.integer64")

#' @export
#' @method vec_cast.integer64 default
vec_cast.integer64.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  # Don't use `vec_default_cast()` because integer64 is not compatible
  # with `vec_init()`
  if (is_unspecified(x)) {
    bit64::as.integer64(unclass(x))
  } else {
    stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }
}

#' @export
#' @method vec_cast.integer64 integer64
vec_cast.integer64.integer64 <- function(x, to, ...) x

#' @export
#' @method vec_cast.integer64 integer
vec_cast.integer64.integer <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.integer integer64
vec_cast.integer.integer64 <- function(x, to, ...) {
  as.integer(x)
}

#' @export
#' @method vec_cast.integer64 logical
vec_cast.integer64.logical <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.logical integer64
vec_cast.logical.integer64 <- function(x, to, ...) {
  as.logical(x)
}

#' @export
#' @method vec_cast.integer64 character
vec_cast.integer64.character <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.character integer64
vec_cast.character.integer64 <- function(x, to, ...) {
  as.character(x)
}

#' @export
#' @method vec_cast.integer64 double
vec_cast.integer64.double <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.double integer64
vec_cast.double.integer64 <- function(x, to, ...) {
  as.double(x)
}
