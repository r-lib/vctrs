
#' 64 bit integers
#'
#' A `integer64` vector is 64 bits integer vector. Details
#' are implented in the `bit64` package.
#'
#' These functions help the `integer64` class from `bit64` in to
#' the vctrs type system by providing constructors, coercion functions,
#' and casting functions. `new_int64()` is a low-level
#' constructor that only checks the type is valid, so
#' is for expert use only.
#'
#' @param x 64 bit integer vector
#' @param ...,class Used to for subclasses.
#' @keywords internal
#' @export
new_int64 <- function(x = bit64::integer64(), ..., class = character()) {
  stopifnot(inherits(x, "integer64"))
  x
}

# Print -------------------------------------------------------------------

#' @rdname new_int64
#' @export
vec_ptype_full.integer64 <- function(x) {
  "integer64"
}

#' @rdname new_int64
#' @export
vec_ptype_abbr.integer64 <- function(x) {
  "int64"
}


# Coerce ------------------------------------------------------------------

#' @export
#' @rdname new_int64
#' @export vec_type2.integer64
#' @method vec_type2 integer64
vec_type2.integer64 <- function(x, y) {
  UseMethod("vec_type2.integer64", y)
}

#' @method vec_type2.integer64 default
#' @export
vec_type2.integer64.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.integer64 integer64
#' @export
vec_type2.integer64.integer64 <- function(x, y) bit64::integer64()


#' @method vec_type2.integer64 integer
#' @export
vec_type2.integer64.integer <- function(x, y) bit64::integer64()

#' @method vec_type2.integer integer64
#' @export
vec_type2.integer.integer64 <- function(x, y) bit64::integer64()

#' @method vec_type2.integer64 logical
#' @export
vec_type2.integer64.logical <- function(x, y) bit64::integer64()

#' @method vec_type2.logical integer64
#' @export
vec_type2.logical.integer64 <- function(x, y) bit64::integer64()


# Cast --------------------------------------------------------------------

#' @export
#' @rdname new_int64
#' @export vec_cast.integer64
#' @method vec_cast integer64
vec_cast.integer64 <- function(x, to) UseMethod("vec_cast.integer64")

#' @export
#' @method vec_cast.integer64 default
vec_cast.integer64.default <- function(x, to) stop_incompatible_cast(x, to)

#' @export
#' @method vec_cast.integer64 integer64
vec_cast.integer64.integer64 <- function(x, to) x

#' @export
#' @method vec_cast.integer64 integer
vec_cast.integer64.integer <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.integer integer64
vec_cast.integer.integer64 <- function(x, to) {
  as.integer(x)
}

#' @export
#' @method vec_cast.integer64 logical
vec_cast.integer64.logical <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.logical integer64
vec_cast.logical.integer64 <- function(x, to) {
  as.logical(x)
}

#' @export
#' @method vec_cast.integer64 character
vec_cast.integer64.character <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.character integer64
vec_cast.character.integer64 <- function(x, to) {
  as.character(x)
}

#' @export
#' @method vec_cast.integer64 double
vec_cast.integer64.double <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.logical integer64
vec_cast.double.integer64 <- function(x, to) {
  as.double(x)
}
