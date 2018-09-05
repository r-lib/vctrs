#' Return the underlying data from a vector
#'
#' Currently, due to the underlying memory architecture of vectors, this
#' requires a full copy of the underlying data.
#'
#' @param x A vector
#' @export
#' @return The data underlying `x`, free from an attributes.
vec_data <- function(x) {
  stopifnot(is_vector(x))

  # TODO: implement with ALTREP to avoid making a copy
  if (is_record(x)) {
    attributes(x) <- list(names = fields(x))
  } else {
    attributes(x) <- list(names = names(x))
  }

  x
}

is_record <- function(x) {
  UseMethod("is_record")
}
is_record.POSIXlt <- function(x) TRUE
is_record.record <- function(x) TRUE
is_record.default <- function(x) FALSE

