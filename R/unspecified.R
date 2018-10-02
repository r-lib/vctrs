#' A 1d vector of unspecified type
#'
#' This is a [partial type](new_partial) used to represent logical vectors
#' that only contain `NA`. These require special handling because we want to
#' allow `NA` to specify missingness without requiring a type.
#'
#' `vec_unspecified_cast()` is a helper to use in your [vec_cast()] methods.
#' See `vignette("s3-vector")` for detail.
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
unspecified <- function(n = 0) {
  structure(rep(NA, n), class = "vctrs_unspecified")
}

#' @export
`[.vctrs_unspecified` <- function(x, i, ...) {
  unspecified(length(NextMethod()))
}

#' @export
print.vctrs_unspecified <- function(x, ...) {
  cat("<unspecified> [", length(x), "]\n", sep = "")
}

is_unspecified <- function(x) {
  .Call(vctrs_is_unspecified, x)
}

#' @export
#' @rdname unspecified
vec_unspecified_cast <- function(x, to) {
  if (is_unspecified(x)) {
    vec_na(to, length(x))
  } else {
    stop_incompatible_cast(x, to)
  }
}

#' @export
vec_type_finalise.vctrs_unspecified <- function(x) {
  logical()
}

# Type system -------------------------------------------------------------

#' @rdname unspecified
#' @export vec_type2.vctrs_unspecified
#' @method vec_type2 vctrs_unspecified
#' @export
vec_type2.vctrs_unspecified <- function(x, y) UseMethod("vec_type2.vctrs_unspecified", y)
#' @method vec_type2.vctrs_unspecified default
#' @export
vec_type2.vctrs_unspecified.default <- function(x, y) y
#' @method vec_type2.vctrs_unspecified vctrs_unspecified
#' @export
vec_type2.vctrs_unspecified.vctrs_unspecified <- function(x, y) x

#' @method vec_type2.logical vctrs_unspecified
#' @export
vec_type2.logical.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified logical
#' @export
vec_type2.vctrs_unspecified.logical <- function(x, y) y
#' @method vec_type2.integer vctrs_unspecified
#' @export
vec_type2.integer.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified integer
#' @export
vec_type2.vctrs_unspecified.integer <- function(x, y) y
#' @method vec_type2.double vctrs_unspecified
#' @export
vec_type2.double.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified double
#' @export
vec_type2.vctrs_unspecified.double <- function(x, y) y
#' @method vec_type2.character vctrs_unspecified
#' @export
vec_type2.character.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified character
#' @export
vec_type2.vctrs_unspecified.character <- function(x, y) y
#' @method vec_type2.factor vctrs_unspecified
#' @export
vec_type2.factor.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.ordered vctrs_unspecified
#' @export
vec_type2.ordered.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified factor
#' @export
vec_type2.vctrs_unspecified.factor <- function(x, y) y
#' @method vec_type2.list vctrs_unspecified
#' @export
vec_type2.list.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified list
#' @export
vec_type2.vctrs_unspecified.list <- function(x, y) y
#' @method vec_type2.vctrs_list_of vctrs_unspecified
#' @export
vec_type2.vctrs_list_of.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified vctrs_list_of
#' @export
vec_type2.vctrs_unspecified.vctrs_list_of <- function(x, y) y
#' @method vec_type2.Date vctrs_unspecified
#' @export
vec_type2.Date.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified Date
#' @export
vec_type2.vctrs_unspecified.Date <- function(x, y) y
#' @method vec_type2.POSIXt vctrs_unspecified
#' @export
vec_type2.POSIXt.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified POSIXt
#' @export
vec_type2.vctrs_unspecified.POSIXt <- function(x, y) y
#' @method vec_type2.difftime vctrs_unspecified
#' @export
vec_type2.difftime.vctrs_unspecified <- function(x, y) x
#' @method vec_type2.vctrs_unspecified difftime
#' @export
vec_type2.vctrs_unspecified.difftime <- function(x, y) y
