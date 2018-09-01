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
  if (inherits(x, "record")) {
    attributes(x) <- list(names = fields(x))
  } else {
    attributes(x) <- list(names = names(x))
  }

  x
}
