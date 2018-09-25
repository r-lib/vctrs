#' Extract underlying data
#'
#' Extract the data underlying an S3 vector object, i.e. the underlying
#' atomic vector or list. Currently, due to the underlying memory architecture
#' of R, this creates a full copy of the underlying data.
#'
#' @param x A vector
#' @export
#' @return The data underlying `x`, free from an attributes.
#' @seealso See [vec_restore()] for the inverse operation: it restores
#'   attributes given a bare vector and a prototype;
#'   `vec_restore(vec_data(x), x)` will always yield `x`.
vec_data <- function(x) {
  if (!is_vector(x)) {
    return(x)
  }

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
is_record.vctrs_rcrd <- function(x) TRUE
is_record.default <- function(x) FALSE

