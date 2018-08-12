#' Compute and alter "shape" of a vector
#'
#' The shape of a vector is its `vec_dim()` (i.e. `dim()` or `length()`)
#' and whether or not it's a data frame.
#'
#' @param x For `shape()`, a vector giving dimensions of an object.
#'   For `as_vec_shape()`, an existing data vector
#' @param data.frame Does this shape represent a data frame? Data frames
#'   must be 2 dimensional, and can only be recycled along the first dimension.
#' @keywords internal
#' @export
#' @examples
#' vec_shape(1:10)
#' vec_shape(1:10, 1)
#' \dontrun{
#' vec_shape(1:10, 1)
#' }
#'
#' vec_shape(mtcars)
vec_shape <- function(..., data.frame = FALSE) {
  args <- list2(...)
  dims <- map(args, as_vec_shape)
  reduce(dims, vec_shape2, .init = NULL)
}

new_vec_shape <- function(x, data.frame = FALSE) {
  stopifnot(is.numeric(x))

  structure(
    x,
    data.frame = data.frame,
    class = "vec_shape"
  )
}

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
  cat_line(
    "shape: [", paste0(x, collapse = ","), "]",
    if (attr(x, "data.frame")) " (df)"
  )
  invisible(x)
}

# Implementation ----------------------------------------------------------

vec_shape2 <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else if (is.null(y)) {
    return(x)
  }

  dim <- one_pad(x, y)

  if (xor(attr(x, "data.frame"), attr(y, "data.frame"))) {
    if (length(dim$x) > 2) {
      stop("Data frames must be 2d", call. = FALSE)
    }
  }
  if (attr(x, "data.frame") && dim$y[2] > dim$x[2]) {
    stop("Can't expand columns of data frame", call. = FALSE)
  }
  if (attr(y, "data.frame") && dim$x[2] > dim$y[2]) {
    stop("Can't expand columns of data frame", call. = FALSE)
  }

  new_vec_shape(map2_int(dim$x, dim$y, recycle_length))
}

#' @rdname vec_shape
#' @export
vec_reshape <- function(x, shape) {
  if (is.null(x) || is.null(shape))
    return(x)

  if (is.data.frame(x)) {
    if (length(shape) != 2L || shape[[2]] != ncol(x)) {
      stop("Can only recycle data frame along rows", call. = FALSE)
    }

    vec_rep(x, shape[1])
  } else if (vec_dims(x) == 1 && length(shape) == 1) {
    # vector recycling
    if (length(x) != shape && length(x) != 1L && shape != 0L) {
      stop(
        "Can't recycle vector of length ", length(x), " to length ", shape,
        call. = FALSE
      )
    }
    vec_rep(x, shape)
  } else if (length(x) == 1) {
    # scalar recycling
    array(x, dim = as.numeric(shape))
  } else {
    if (vec_dims(x) > length(shape)) {
      stop("Can only increase dimensionality of vector", call. = FALSE)
    }

    dim(x) <- one_pad(vec_dim(x), shape)$x
    recycled <- dim(x) != shape

    indices <- rep(list(missing_arg()), length(shape))
    indices[recycled] <- map(shape[recycled], rep_len, x = 1L)

    eval_bare(expr(x[!!!indices, drop = FALSE]))
  }
}

# Helpers -----------------------------------------------------------------

recycle_length <- function(nx, ny) {
  if (nx == ny) {
    nx
  } else if (nx == 0L || ny == 0L) {
    0L
  } else if (nx == 1L || ny == 1L) {
    max(nx, ny)
  } else {
    stop("Incompatible lengths: ", nx, ", ", ny, call. = FALSE)
  }
}

one_pad <- function(x, y) {
  nx <- length(x)
  ny <- length(y)

  if (nx == ny) {
    list(x = x, y = y)
  } else if (nx < ny) {
    list(x = c(x, rep(1L, ny - nx)), y = y)
  } else {
    list(x = x, y = c(y, rep(1L, nx - ny)))
  }
}
