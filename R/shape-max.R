vec_shape2 <- function(x, y) {
  x <- as_vec_shape(x)
  y <- as_vec_shape(y)

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
