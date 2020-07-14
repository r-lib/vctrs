# Type2 -------------------------------------------------------------------

# Left generics -----------------------------------------------------------

#' @rdname vec_ptype2
#' @export vec_ptype2.logical
#' @method vec_ptype2 logical
#' @export
vec_ptype2.logical <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.logical")
}
#' @rdname vec_ptype2
#' @export vec_ptype2.integer
#' @method vec_ptype2 integer
#' @export
vec_ptype2.integer <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.integer")
}
#' @rdname vec_ptype2
#' @export vec_ptype2.double
#' @method vec_ptype2 double
#' @export
vec_ptype2.double <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.double")
}
#' @rdname vec_ptype2
#' @export vec_ptype2.complex
#' @method vec_ptype2 complex
#' @export
vec_ptype2.complex <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.complex")
}
#' @rdname vec_ptype2
#' @export vec_ptype2.character
#' @method vec_ptype2 character
#' @export
vec_ptype2.character <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.character")
}
#' @rdname vec_ptype2
#' @export vec_ptype2.raw
#' @method vec_ptype2 raw
#' @export
vec_ptype2.raw <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.raw")
}
#' @rdname vec_ptype2
#' @export vec_ptype2.list
#' @method vec_ptype2 list
#' @export
vec_ptype2.list <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.list")
}


# Numeric-ish

#' @method vec_ptype2.logical logical
#' @export
vec_ptype2.logical.logical <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.logical.logical")
}

#' @export
#' @method vec_ptype2.integer integer
vec_ptype2.integer.integer <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.integer.integer")
}
#' @export
#' @method vec_ptype2.logical integer
vec_ptype2.logical.integer <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.logical.integer")
}
#' @export
#' @method vec_ptype2.integer logical
vec_ptype2.integer.logical <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.integer.logical")
}

#' @export
#' @method vec_ptype2.double double
vec_ptype2.double.double <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.double.double")
}
#' @export
#' @method vec_ptype2.logical double
vec_ptype2.logical.double <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.logical.double")
}
#' @export
#' @method vec_ptype2.double logical
vec_ptype2.double.logical <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.double.logical")
}
#' @export
#' @method vec_ptype2.integer double
vec_ptype2.integer.double <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.integer.double")
}
#' @export
#' @method vec_ptype2.double integer
vec_ptype2.double.integer <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.double.integer")
}

#' @export
#' @method vec_ptype2.complex complex
vec_ptype2.complex.complex <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.complex.complex")
}
#' @export
#' @method vec_ptype2.integer complex
vec_ptype2.integer.complex <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.integer.complex")
}
#' @export
#' @method vec_ptype2.complex integer
vec_ptype2.complex.integer <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.complex.integer")
}
#' @export
#' @method vec_ptype2.double complex
vec_ptype2.double.complex <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.double.complex")
}
#' @export
#' @method vec_ptype2.complex double
vec_ptype2.complex.double <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.complex.double")
}



# Character

#' @method vec_ptype2.character character
#' @export
vec_ptype2.character.character <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.character.character")
}


# Raw

#' @export
#' @method vec_ptype2.raw raw
vec_ptype2.raw.raw <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.raw.raw")
}


# Lists

#' @method vec_ptype2.list list
#' @export
vec_ptype2.list.list <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_native_implementation("vec_ptype2.list.list")
}


# Cast --------------------------------------------------------------------

# These methods for base types are handled at the C level unless
# inputs have shape or have lossy casts

#' @export
#' @rdname vec_cast
#' @export vec_cast.logical
#' @method vec_cast logical
vec_cast.logical <- function(x, to, ...) {
  UseMethod("vec_cast.logical")
}
#' @export
#' @method vec_cast.logical logical
vec_cast.logical.logical <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}
#' @export
#' @method vec_cast.logical integer
vec_cast.logical.integer <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- vec_coerce_bare(x, "logical")
  out <- shape_broadcast(out, to, x_arg = x_arg, to_arg = to_arg)
  lossy <- !x %in% c(0L, 1L, NA_integer_)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.logical double
vec_cast.logical.double <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- vec_coerce_bare(x, "logical")
  out <- shape_broadcast(out, to, x_arg = x_arg, to_arg = to_arg)
  lossy <- !x %in% c(0, 1, NA_real_)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.integer
#' @method vec_cast integer
vec_cast.integer <- function(x, to, ...) {
  UseMethod("vec_cast.integer")
}
#' @export
#' @method vec_cast.integer logical
vec_cast.integer.logical <- function(x, to, ...) {
  x <- vec_coerce_bare(x, "integer")
  shape_broadcast(x, to, ...)
}
#' @export
#' @method vec_cast.integer integer
vec_cast.integer.integer <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}
#' @export
#' @method vec_cast.integer double
vec_cast.integer.double <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- suppressWarnings(vec_coerce_bare(x, "integer"))
  x_na <- is.na(x)
  lossy <- (out != x & !x_na) | xor(x_na, is.na(out))
  out <- shape_broadcast(out, to, x_arg = x_arg, to_arg = to_arg)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.double
#' @method vec_cast double
vec_cast.double <- function(x, to, ...) {
  UseMethod("vec_cast.double")
}
#' @export
#' @method vec_cast.double logical
vec_cast.double.logical <- function(x, to, ...) {
  x <- vec_coerce_bare(x, "double")
  shape_broadcast(x, to, ...)
}
#' @export
#' @method vec_cast.double integer
vec_cast.double.integer <- vec_cast.double.logical
#' @export
#' @method vec_cast.double double
vec_cast.double.double <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.complex
#' @method vec_cast complex
vec_cast.complex <- function(x, to, ...) {
  UseMethod("vec_cast.complex")
}
#' @export
#' @method vec_cast.complex logical
vec_cast.complex.logical <- function(x, to, ...) {
  x <- vec_coerce_bare(x, "complex")
  shape_broadcast(x, to, ...)
}
#' @export
#' @method vec_cast.complex integer
vec_cast.complex.integer <- vec_cast.complex.logical
#' @export
#' @method vec_cast.complex double
vec_cast.complex.double <- vec_cast.complex.logical
#' @export
#' @method vec_cast.complex complex
vec_cast.complex.complex <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.raw
#' @method vec_cast raw
vec_cast.raw <- function(x, to, ...) {
  UseMethod("vec_cast.raw")
}
#' @export
#' @method vec_cast.raw raw
vec_cast.raw.raw <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.character
#' @method vec_cast character
vec_cast.character <- function(x, to, ...) {
  UseMethod("vec_cast.character")
}
#' @export
#' @method vec_cast.character character
vec_cast.character.character <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}

#' @rdname vec_cast
#' @export vec_cast.list
#' @method vec_cast list
#' @export
vec_cast.list <- function(x, to, ...) {
  UseMethod("vec_cast.list")
}
#' @export
#' @method vec_cast.list list
vec_cast.list.list <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}


# compare ------------------------------------------------------------

#' @export
vec_proxy_compare.raw <- function(x, ...) {
  # because:
  # order(as.raw(1:3))
  # #> Error in order(as.raw(1:3)): unimplemented type 'raw' in 'orderVector1'
  as.integer(x)
}
