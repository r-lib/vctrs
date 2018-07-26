# This is not reducable!
vecshape_max <- function(x, y) {
  if (is.null(x) && is.null(y)) {
    return(NULL)
  } else if (is.null(x)) {
    return(vec_dim(y))
  } else if (is.null(y)) {
    return(vec_dim(x))
  }

  dim <- one_pad(vec_dim(x), vec_dim(y))

  # Can only recycle a data frame in along rows
  # This logic isn't quite correct yet
  if (is.data.frame(x) || is.data.frame(y)) {
    if (length(dim$x) > 2) {
      stop("Data frames must be 2d")
    }
    if (length(dim$x) == 2 && dim$x[2] != dim$y[2]) {
      stop("Can not recycle columns of data frame, only rows")
    }
  }

  map2_int(dim$x, dim$y, recycle_length)
}

vecshape_coerce <- function(x, shape) {
  # Special cases
  if (is.null(x)) {
    return(NULL)
  } else if (is.null(shape)) {
    return(x)
  } else if (is.data.frame(x)) {
    # data frame recycling
    return(vec_rep(x, shape[1]))
  } else if (vec_dims(x) == 1 && length(shape) == 1) {
    # vector recycling
    return(vec_rep(x, shape))
  } else if (length(x) == 1) {
    # scalar recycling
    return(array(x, dim = shape))
  }

  dim(x) <- one_pad(vec_dim(x), shape)$x

  recycled <- dim(x) != shape

  indices <- rep(list(missing_arg()), length(shape))
  indices[recycled] <- map(shape[recycled], rep_len, x = 1L)

  eval_bare(expr(x[!!!indices, drop = FALSE]))
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
