
#' @export
vec_proxy_equal.integer64 <- function(x, ...) {
  x <- integer64_to_complex(x)
  # Second call to `vec_proxy_equal()` to allow arrays
  # to run through `vec_proxy_equal.array()` and become data frames
  vec_proxy_equal(x)
}

#' @export
vec_proxy_compare.integer64 <- function(x, ...) {
  # Can't return a complex proxy, as complex types aren't comparable.
  # TODO: Should complex proxies be comparable (real then imaginary)?
  # We could supply a `vec_proxy_compare.complex()` that would abort?

  if (has_dim(x)) {
    # Convert array to data frame, then proxy each atomic integer64 column
    x <- as.data.frame(x)
    x <- vec_proxy_compare(x)
    return(x)
  }

  x <- integer64_to_complex(x)

  data_frame(high = Re(x), low = Im(x))
}

#' @export
vec_proxy_order.integer64 <- function(x, ...) {
  # Complex types are orderable by `vec_order()`, so use simpler equality proxy
  vec_proxy_equal.integer64(x, ...)
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

integer64_to_complex <- function(x) {
  .Call(vctrs_integer64_to_complex, x)
}
complex_to_integer64 <- function(x) {
  .Call(vctrs_complex_to_integer64, x)
}

# Utilities for displaying the conversion.
# R's print method for `complex()` is a bit buggy, and it is more
# reliable to extract the real/imaginary parts into their own columns.
show_integer64_to_complex <- function(x) {
  output <- integer64_to_complex(x)
  data.frame(input = x, left = Re(output), right = Im(output))
}
show_complex_to_integer64 <- function(x) {
  output <- complex_to_integer64(x)
  data.frame(output = output, left = Re(x), right = Im(x))
}
