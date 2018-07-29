vec_cast <- function(x, to) {
  UseMethod("vec_cast", to)
}

# Base vectors --------------------------------------------------------------

#' @export
vec_cast.NULL <- function(x, to) {
  x
}

#' @export
vec_cast.logical <- function(x, to) {
  set_names(as.logical(x), names(x))
}

#' @export
vec_cast.integer <- function(x, to) {
  set_names(as.integer(x), names(x))
}

#' @export
vec_cast.double <- function(x, to) {
  set_names(as.double(x), names(x))
}

#' @export
vec_cast.character <- function(x, to) {
  set_names(as.character(x), names(x))
}

#' @export
vec_cast.list <- function(x, to) {
  as.list(x)
}

# S3 vectors --------------------------------------------------------------

#' @export
vec_cast.factor <- function(x, to) {
  factor(as.character(x), levels = levels(to))
}

#' @export
vec_cast.difftime <- function(x, to) {
  structure(
    as.double(x),
    class = "difftime",
    units = units(to)
  )
}

#' @export
vec_cast.Date <- function(x, to) {
  as.Date(x)
}

#' @export
vec_cast.POSIXt <- function(x, to) {
  as.POSIXct(x)
}

#' @export
vec_cast.data.frame <- function(x, to) {
  # Coerce common columns
  common <- intersect(names(x), names(to))
  x[common] <- map2(x[common], to[common], vec_cast)

  # Add new columns
  only_type <- setdiff(names(to), names(x))
  x[only_type] <- map(to[only_type], vec_na, n = vec_length(x))

  x[c(common, only_type)]
}
