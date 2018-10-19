#' @export
vec_ptype_abbr.integer64 <- function(x) {
  "int64"
}

#------- vec_cast

#' @export
#' @rdname vec_cast
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
#' @method vec_cast.integer64 character
vec_cast.integer64.character <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.integer64 double
vec_cast.integer64.double <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.integer64 logical
vec_cast.integer64.logical <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.integer64 factor
vec_cast.integer64.factor <- function(x, to) {
  bit64::as.integer64(x)
}


#' @export
#' @method vec_cast.character integer64
vec_cast.character.integer64 <- function(x, to) {
  bit64::as.character.integer64(x)
}

#' @export
#' @method vec_cast.double integer64
vec_cast.double.integer64 <- function(x, to) {
  bit64::as.double.integer64(x)
}

#' @export
#' @method vec_cast.integer integer64
vec_cast.integer.integer64 <- function(x, to) {
  bit64::as.integer.integer64(x)
}

#' @export
#' @method vec_cast.logical integer64
vec_cast.logical.integer64 <- function(x, to) {
  bit64::as.logical.integer64(x)
}

#--------- vec_type2

#' @export
#' @rdname vec_type2
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

#' @method vec_type2.integer64 double
#' @export
vec_type2.integer64.double <- function(x, y) bit64::integer64()

#' @method vec_type2.integer64 logical
#' @export
vec_type2.integer64.logical <- function(x, y) bit64::integer64()

#' @method vec_type2.integer64 character
#' @export
vec_type2.integer64.character <- function(x, y) character()


#' @method vec_type2.integer integer64
#' @export
vec_type2.integer.integer64 <- function(x, y) bit64::integer64()

#' @method vec_type2.double integer64
#' @export
vec_type2.double.integer64 <- function(x, y) bit64::integer64()

#' @method vec_type2.character integer64
#' @export
vec_type2.character.integer64 <- function(x, y) character()

#' @method vec_type2.logical integer64
#' @export
vec_type2.logical.integer64 <- function(x, y) bit64::integer64()
