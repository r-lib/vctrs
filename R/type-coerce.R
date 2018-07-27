# Behaviour is undefined if x is not vectype_max(x, val)
vectype_coerce <- function(x, val) {
  UseMethod("vectype_coerce")
}

# Base vectors --------------------------------------------------------------

#' @export
vectype_coerce.NULL <- function(x, val) {
  val
}

#' @export
vectype_coerce.logical <- function(x, val) {
  set_names(as.logical(val), names(val))
}

#' @export
vectype_coerce.integer <- function(x, val) {
  set_names(as.integer(val), names(val))
}

#' @export
vectype_coerce.double <- function(x, val) {
  set_names(as.double(val), names(val))
}

#' @export
vectype_coerce.character <- function(x, val) {
  set_names(as.character(val), names(val))
}

#' @export
vectype_coerce.list <- function(x, val) {
  as.list(val)
}

# S3 vectors --------------------------------------------------------------

#' @export
vectype_coerce.factor <- function(x, val) {
  factor(as.character(val), levels = levels(x))
}

#' @export
vectype_coerce.difftime <- function(x, val) {
  structure(
    as.double(val),
    class = "difftime",
    units = units(x)
  )
}

#' @export
vectype_coerce.Date <- function(x, val) {
  as.Date(val)
}

#' @export
vectype_coerce.POSIXt <- function(x, val) {
  as.POSIXct(val)
}

#' @export
vectype_coerce.data.frame <- function(x, val) {
  # Coerce common columns
  common <- intersect(names(x), names(val))
  val[common] <- map2(x[common], val[common], vectype_coerce)

  # Add new columns
  only_type <- setdiff(names(x), names(val))
  val[only_type] <- map(x[only_type], vec_na, n = vec_length(val))

  val[c(common, only_type)]
}
