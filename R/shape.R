#' Compute "shape" of a vector
#'
#' The shape of a vector is its `vec_dim()`, i.e. the dimension attribute,
#' if it exists, otherwise the length, and whether or not its a data frame.
#' `new_vec_shape()` is the low-level constructor, `vec_shape()` is a helper for
#' interactive exploration, `as_vec_shape()` computes the shape of an existing
#' object. Use `max()` to compute the "maximum" shape of a set of shapes,
#' obeying the recycling rules.
#'
#' @param x For `shape()`, a vector giving dimensions of an object.
#'   For `as_vec_shape()`, an existing data vector
#' @param data.frame Does this shape represent a data frame? Data frames
#'   must be 2 dimensional, and can only be recycled along the first dimension.
#' @keywords internal
#' @export
#' @examples
#' vec_shape(1, 2, 3)
#' as_vec_shape(mtcars)
#'
#' max(vec_shape(1), vec_shape(10))
#' \dontrun{
#' max(vec_shape(2), vec_shape(10))
#' }
#'
#' max(vec_shape(10), vec_shape(0))
#' max(vec_shape(10), vec_shape(10, 2))
#' max(vec_shape(1, 5, 1), vec_shape(2, 5, 10))
vec_shape <- function(..., data.frame = FALSE) {
  new_vec_shape(c(...), data.frame = data.frame)
}

#' @export
#' @rdname vec_shape
new_vec_shape <- function(x, data.frame = FALSE) {
  stopifnot(is.numeric(x))

  structure(
    x,
    data.frame = data.frame,
    class = "vec_shape"
  )
}

#' @export
#' @rdname vec_shape
as_vec_shape <- function(x) UseMethod("as_vec_shape")

#' @export
as_vec_shape.vec_shape <- function(x) x

#' @export
as_vec_shape.data.frame <- function(x) {
  new_vec_shape(vec_dim(x), data.frame = TRUE)
}

#' @export
as_vec_shape.NULL <- function(x) NULL

#' @export
as_vec_shape.default <- function(x) {
  if (is_vector(x)) {
    new_vec_shape(vec_dim(x))
  } else {
    stop("Can only compute shape of a vector", call. = FALSE)
  }
}

#' @export
print.vec_shape <- function(x, ...) {
  cat("shape: [", paste0(x, collapse = ","), "]\n", sep = "")
  invisible(x)
}

#' @export
max.vec_shape <- function(..., na.rm = FALSE) {
  args <- list2(...)
  reduce(args, vecshape_max, .init = NULL)
}
