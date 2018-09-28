#' A 1d vector of unknown type
#'
#' This is a [partial type](new_partial) used to represents logical vectors
#' that only contain `NA`. These require special handling because we want to
#' allow `NA` to specify missingness without requiring a type.
#'
#' @keywords internal
#' @param n Length of vector
#' @export
#' @examples
#' vec_ptype()
#' vec_ptype(NA)
#'
#' vec_c(NA, factor("x"))
#' vec_c(NA, Sys.Date())
#' vec_c(NA, Sys.time())
#' vec_c(NA, list(1:3, 4:5))
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

# In practice, a vector containing only NA implies shape, but not type.
is_nullish <- function(x) {
  is.null(x) || (is.logical(x) && vec_dims(x) == 1L && length(x) > 0 && all(is.na(x)))
}

vec_unknown_cast <- function(x, to) {
  if (is_nullish(x)) {
    vec_na(to, length(x))
  } else {
    stop_incompatible_cast(x, to)
  }
}

#' @export
vec_type_finalise.unknown <- function(x) {
  logical()
}

# Type system -------------------------------------------------------------

#' @rdname unknown
#' @export vec_type2.unknown
#' @method vec_type2 unknown
#' @export
vec_type2.unknown <- function(x, y) UseMethod("vec_type2.unknown", y)
#' @method vec_type2.unknown default
#' @export
vec_type2.unknown.default <- function(x, y) stop_incompatible_type(x, y)
#' @method vec_type2.unknown unknown
#' @export
vec_type2.unknown.unknown <- function(x, y) x

#' @method vec_type2.logical unknown
#' @export
vec_type2.logical.unknown <- function(x, y) x
#' @method vec_type2.unknown logical
#' @export
vec_type2.unknown.logical <- function(x, y) y
#' @method vec_type2.integer unknown
#' @export
vec_type2.integer.unknown <- function(x, y) x
#' @method vec_type2.unknown integer
#' @export
vec_type2.unknown.integer <- function(x, y) y
#' @method vec_type2.double unknown
#' @export
vec_type2.double.unknown <- function(x, y) x
#' @method vec_type2.unknown double
#' @export
vec_type2.unknown.double <- function(x, y) y
#' @method vec_type2.character unknown
#' @export
vec_type2.character.unknown <- function(x, y) x
#' @method vec_type2.unknown character
#' @export
vec_type2.unknown.character <- function(x, y) y
#' @method vec_type2.factor unknown
#' @export
vec_type2.factor.unknown <- function(x, y) x
#' @method vec_type2.ordered unknown
#' @export
vec_type2.ordered.unknown <- function(x, y) x
#' @method vec_type2.unknown factor
#' @export
vec_type2.unknown.factor <- function(x, y) y
#' @method vec_type2.list unknown
#' @export
vec_type2.list.unknown <- function(x, y) x
#' @method vec_type2.unknown list
#' @export
vec_type2.unknown.list <- function(x, y) y
#' @method vec_type2.vctrs_list_of unknown
#' @export
vec_type2.vctrs_list_of.unknown <- function(x, y) x
#' @method vec_type2.unknown vctrs_list_of
#' @export
vec_type2.unknown.vctrs_list_of <- function(x, y) y
#' @method vec_type2.Date unknown
#' @export
vec_type2.Date.unknown <- function(x, y) x
#' @method vec_type2.unknown Date
#' @export
vec_type2.unknown.Date <- function(x, y) y
#' @method vec_type2.POSIXt unknown
#' @export
vec_type2.POSIXt.unknown <- function(x, y) x
#' @method vec_type2.unknown POSIXt
#' @export
vec_type2.unknown.POSIXt <- function(x, y) y
#' @method vec_type2.difftime unknown
#' @export
vec_type2.difftime.unknown <- function(x, y) x
#' @method vec_type2.unknown difftime
#' @export
vec_type2.unknown.difftime <- function(x, y) y
