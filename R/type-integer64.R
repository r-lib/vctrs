#' @export
vec_proxy_equal.integer64 <- function(x, ...) {
  if (is.array(x)) {
    # Stopgap to convert arrays to data frames, then run them through
    # `vec_proxy_equal()` again, which will proxy each column
    x <- as.data.frame(x)
    x <- vec_proxy_equal(x)
    return(x)
  }

  integer64_proxy(x)
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
  UseMethod("vec_ptype2.integer64")
}

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
vec_cast.integer64 <- function(x, to, ...) {
  UseMethod("vec_cast.integer64")
}

#' @export
#' @method vec_cast.integer64 integer64
vec_cast.integer64.integer64 <- function(x, to, ...) {
  x
}

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
#' @method vec_cast.integer64 double
vec_cast.integer64.double <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.double integer64
vec_cast.double.integer64 <- function(x, to, ...) {
  as.double(x)
}

# ------------------------------------------------------------------------------

integer64_proxy <- function(x) {
  .Call(vctrs_integer64_proxy, x)
}
integer64_restore <- function(x) {
  .Call(vctrs_integer64_restore, x)
}
