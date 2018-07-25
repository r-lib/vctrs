
# Unlike base R, no vector has `NULL` dimensions. `vec_length()` is
# equivalent to `NROW()`, but has a name that is easily pronounced.
# `vec_dims()` gives the dimensionality of a vector, i.e. the number
# of dimensions
vec_length <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    length(x)
  } else {
    d[[1]]
  }
}

vec_dim <- function(x) {
  dim(x) %||% length(x)
}

vec_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}
