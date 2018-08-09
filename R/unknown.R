#' A vector of unknown type
#'
#' This is a special vector type used to represent of known length but
#' unknown type.
#'
#' @keywords internal
#' @param n Length of vector
unknown <- function(n = 0) {
  structure(rep(NA, n), class = "unknown")
}

#' @export
`[.unknown` <- function(x, i, ...) {
  unknown(length(NextMethod()))
}

# In practice, a vector containing only NA implies shape, but not type.
is_nullish <- function(x) {
  is.null(x) || (is.logical(x) && vec_dims(x) == 1L && length(x) > 0 && all(is.na(x)))
}

# Type system -------------------------------------------------------------

#' @rdname unknown
#' @export vec_type2.unknown
#' @method vec_type2 unknown
#' @export
vec_type2.unknown <- function(x, y) UseMethod("vec_type2.unknown", y)
#' @method vec_type2.unknown NULL
#' @export
vec_type2.unknown.NULL    <- function(x, y) unknown()
#' @method vec_type2.unknown unknown
#' @export
vec_type2.unknown.unknown <- function(x, y) unknown()
#' @method vec_type2.unknown default
#' @export
vec_type2.unknown.default  <- function(x, y) {
  y
}
#' @method vec_type2.logical unknown
#' @export
vec_type2.logical.unknown <- function(x, y) x
#' @method vec_type2.integer unknown
#' @export
vec_type2.integer.unknown <- function(x, y) x
#' @method vec_type2.double unknown
#' @export
vec_type2.double.unknown <- function(x, y) x
#' @method vec_type2.character unknown
#' @export
vec_type2.character.unknown <- function(x, y) x
#' @method vec_type2.factor unknown
#' @export
vec_type2.factor.unknown <- function(x, y) x
#' @method vec_type2.ordered unknown
#' @export
vec_type2.ordered.unknown <- function(x, y) x
#' @method vec_type2.list unknown
#' @export
vec_type2.list.unknown <- function(x, y) x
#' @method vec_type2.list_of unknown
#' @export
vec_type2.list_of.unknown <- function(x, y) x
#' @method vec_type2.Date unknown
#' @export
vec_type2.Date.unknown <- function(x, y) x
#' @method vec_type2.POSIXt unknown
#' @export
vec_type2.POSIXt.unknown <- function(x, y) x
#' @method vec_type2.data.frame unknown
#' @export
vec_type2.data.frame.unknown <- function(x, y) x

#' @method vec_cast unknown
#' @export
vec_cast.unknown <- function(x, to) {
  x
}
