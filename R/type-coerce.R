# Behaviour is undefined if x is not vectype_max(x, val)
vectype_coerce <- function(x, val) {
  UseMethod("vectype_coerce")
}

# Base vectors --------------------------------------------------------------

vectype_coerce.NULL <- function(x, val) {
  val
}

vectype_coerce.logical <- function(x, val) {
  mode(val) <- "logical"
  val
}

vectype_coerce.integer <- function(x, val) {
  mode(val) <- "integer"
  val
}

vectype_coerce.double <- function(x, val) {
  mode(val) <- "double"
  val
}

vectype_coerce.character <- function(x, val) {
  mode(val) <- "character"
  val
}

vectype_coerce.list <- function(x, val) {
  as.list(val)
}

# S3 vectors --------------------------------------------------------------

vectype_coerce.factor <- function(x, val) {
  factor(as.character(val), levels = levels(x))
}

vectype_coerce.difftime <- function(x, val) {
  structure(
    unclass(val),
    class = "difftime",
    units = units(x)
  )
}

vectype_coerce.Date <- function(x, val) {
  as.Date(val)
}

vectype_coerce.POSIXt <- function(x, val) {
  as.POSIXct(val)
}
