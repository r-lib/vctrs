new_factor <- function(x = integer(), levels = character(), ..., class = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))

  structure(
    x,
    levels = levels,
    ...,
    class = c(class, "factor")
  )
}

new_ordered <- function(x = integer(), levels = character()) {
  new_factor(x = x, levels = levels, class = "ordered")
}

levels_union <- function(x, y) {
  union(levels(x), levels(y))
}


# Coerce ------------------------------------------------------------------


# Cast --------------------------------------------------------------------


#' @rdname vec_cast
#' @export vec_cast.factor
#' @method vec_cast factor
#' @export
vec_cast.factor <- function(x, to) {
  UseMethod("vec_cast.factor")
}
#' @export
#' @method vec_cast.factor NULL
vec_cast.factor.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.factor factor
vec_cast.factor.factor <- function(x, to) {
  if (length(levels(to)) == 0L) {
    factor(as.character(x), levels = unique(x), ordered = is.ordered(to))
  } else {
    lossy <- !x %in% levels(to)
    if (any(lossy)) {
      warn_lossy_cast(x, to, locations = which(lossy))
    }

    factor(x, levels = levels(to), ordered = is.ordered(to))
  }
}
#' @export
#' @method vec_cast.factor character
vec_cast.factor.character <- vec_cast.factor.factor
#' @export
#' @method vec_cast.factor list
vec_cast.factor.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.factor default
vec_cast.factor.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

