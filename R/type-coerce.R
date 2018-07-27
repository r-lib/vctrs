# Behaviour is undefined if x is not vectype_max(x, val)
vectype_coerce <- function(x, val) {
  UseMethod("vectype_coerce")
}

# Base vectors --------------------------------------------------------------

# Unexported wrapper around Rf_coerceVector()
vec_coerce_bare <- function(x, type) {
  coerce <- env_get(ns_env("rlang"), "vec_coerce")
  coerce(x, type)
}

vectype_coerce.NULL <- function(x, val) {
  val
}

vectype_coerce.logical <- function(x, val) {
  vec_coerce_bare(x, "logical")
}

vectype_coerce.integer <- function(x, val) {
  vec_coerce_bare(x, "integer")
}

vectype_coerce.double <- function(x, val) {
  vec_coerce_bare(x, "double")
}

vectype_coerce.character <- function(x, val) {
  vec_coerce_bare(x, "character")
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
    as.double(val),
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
