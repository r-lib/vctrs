#' Create a shape
#'
#' `new_shape()` is the low-level constructor, `shape()` is a helper for
#' interactive exploration, `as_shape()` computes the shape of an existing
#' object. Use `max()` to compute the "maximum" shape of a set of shapes,
#' obeying the recycling rules.
#'
#' @param x For `shape()`, a vector giving dimensions of an object.
#'   For `as_shape()`, an existing data vector
#' @param data.frame Does this shape represent a data frame? Data frames
#'   must be 2 dimensional, and can only be recycled along the first dimension.
#' @keywords internal
#' @export
#' @examples
#' shape(1, 2, 3)
#' as_shape(mtcars)
#'
#' max(shape(1), shape(10))
#' \dontrun{
#' max(shape(2), shape(10))
#' }
#'
#' max(shape(10), shape(0))
#' max(shape(10), shape(10, 2))
#' max(shape(1, 5, 1), shape(2, 5, 10))
shape <- function(..., data.frame = FALSE) {
  new_shape(c(...), data.frame = data.frame)
}

#' @export
#' @rdname shape
new_shape <- function(x, data.frame = FALSE) {
  stopifnot(is.numeric(x))

  structure(
    x,
    data.frame = data.frame,
    class = "vecshape"
  )
}

#' @export
#' @rdname shape
as_shape <- function(x) UseMethod("as_shape")

#' @export
as_shape.vecshape <- function(x) x

#' @export
as_shape.data.frame <- function(x) new_shape(vec_dim(x), data.frame = TRUE)

#' @export
as_shape.NULL <- function(x) NULL

#' @export
as_shape.default <- function(x) {
  if (is_vector(x)) {
    new_shape(vec_dim(x))
  } else {
    stop("Can only compute shape of a vector", call. = FALSE)
  }
}

#' @export
print.vecshape <- function(x, ...) {
  cat("shape: [", paste0(x, collapse = ","), "]\n", sep = "")
  invisible(x)
}

#' @export
max.vecshape <- function(..., na.rm = FALSE) {
  args <- list2(...)
  reduce(args, vecshape_max, .init = NULL)
}
