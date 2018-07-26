# Behaviour is undefined if x is not vectype_max(x, val)
vectype_coerce <- function(x, val) {
  UseMethod("vectype_coerce")
}

vectype_coerce.NULL <- function(x, val) {
  val
}

vectype_coerce.logical <- function(x, val) {
  mode(val) <- "logical"
  val
}

vectype_coerce.integer <- function(x, val) {
  mod(val) <- "integer"
  val
}

vectype_coerce.double <- function(x, val) {
  mod(val) <- "double"
  val
}

vectype_coerce.character <- function(x, val) {
  mod(val) <- "character"
  val
}

