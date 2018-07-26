new_shape <- function(x, data.frame = FALSE) {
  structure(
    x,
    data.frame = data.frame,
    class = "vecshape"
  )
}

shape <- function(..., data.frame = FALSE) {
  new_shape(c(...), data.frame = data.frame)
}

as_shape <- function(x) UseMethod("as_shape")
as_shape.vecshape <- function(x) x
as_shape.data.frame <- function(x) new_shape(vec_dim(x), data.frame = TRUE)
as_shape.NULL <- function(x) NULL
as_shape.default <- function(x) {
  if (is_vector(x)) {
    new_shape(vec_dim(x))
  } else {
    stop("Can only compute shape of a vector", call. = FALSE)
  }
}

print.vecshape <- function(x, ...) {
  cat("shape: [", paste0(x, collapse = ","), "]\n", sep = "")
  invisible(x)
}

max.vecshape <- function(...) {
  args <- list2(...)
  reduce(args, vecshape_max, .init = NULL)
}
