error_cnd_vctrs <- function(message, .subclass, ...) {
  error_cnd(
    .subclass = c(.subclass, "vctrs_error"),
    ...,
    message = message,
    trace = trace_back()
  )
}

error_cnd_names <- function(message, .subclass, locations, ...) {
  error_cnd_vctrs(
    message,
    .subclass = c(.subclass, "vctrs_error_names"),
    locations = locations,
    ...
  )
}

names_cannot_be_empty <- function(locations) {
  error_cnd_names(
    "Names must not be empty.",
    .subclass = "vctrs_error_names_cannot_be_empty",
    locations = locations
  )
}

names_cannot_be_dot_dot <- function(locations) {
  error_cnd_names(
    "Names must not be of the form `...` or `..j`.",
    .subclass = "vctrs_error_names_cannot_be_dot_dot",
    locations = locations
  )
}

names_must_be_unique <- function(locations) {
  error_cnd_names(
    "Names must be unique.",
    .subclass = "vctrs_error_names_must_be_unique",
    locations = locations
  )
}
