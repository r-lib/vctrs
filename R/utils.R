vec_na <- function(x) {
  if (is.list(x)) {
    # list()[NA_integer_] returns list(NULL)
    NULL
  } else {
    vec_subset(x, NA_integer_)
  }
}

vec_subset <- function(x, i) {
  stopifnot(is.integer(i) || is.character(i))

  d <- vec_dims(x)
  if (d == 1) {
    x[i]
  } else if (d == 2) {
    x[i, , drop = FALSE]
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    eval_bare(expr(x[i, !!!miss_args, drop = FALSE]))
  }
}

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
