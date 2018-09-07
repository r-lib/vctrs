#' A vector of unknown type
#'
#' This is a special vector type used to represent of known length but
#' unknown type.
#'
#' @keywords internal
#' @param n Length of vector
#' @export
#' @examples
#' vec_ptype()
#' vec_ptype(NA)
#'
#' vec_ptype(unknown(), "x")
unknown <- function(n = 0) {
  structure(rep(NA, n), class = "unknown")
}

#' @export
`[.unknown` <- function(x, i, ...) {
  unknown(length(NextMethod()))
}

#' @export
print.unknown <- function(x, ...) {
  cat("<unknown> [", length(x), "]\n", sep = "")
}

#' @export
#' @rdname unknown
is_unknown <- function(x) {
  inherits(x, "unknown")
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
vec_type2.unknown <- function(x, y) {
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
#' @method vec_type2.vctrs_list_of unknown
#' @export
vec_type2.vctrs_list_of.unknown <- function(x, y) x
#' @method vec_type2.Date unknown
#' @export
vec_type2.Date.unknown <- function(x, y) x
#' @method vec_type2.POSIXt unknown
#' @export
vec_type2.POSIXt.unknown <- function(x, y) x
#' @method vec_type2.data.frame unknown
#' @export
vec_type2.data.frame.unknown <- function(x, y) x
#' @method vec_type2.difftime unknown
#' @export
vec_type2.difftime.unknown <- function(x, y) x

#' @method vec_cast unknown
#' @export
vec_cast.unknown <- function(x, to) {
  x
}
