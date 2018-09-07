#' Extract underlying data
#'
#' Extract the data underlying an S3 vector object, i.e. the underlying
#' atomic vector or list. Currently, due to the underlying memory architecture
#' of R, this creates a full copy of the underlying data.
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
is_record.rcrd <- function(x) TRUE
is_record.default <- function(x) FALSE

