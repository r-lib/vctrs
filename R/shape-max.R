vecshape_max <- function(x, y) {
  x <- as_shape(x)
  y <- as_shape(y)

  if (is.null(x) && is.null(y)) {
    return(NULL)
  } else if (is.null(x)) {
    return(y)
  } else if (is.null(y)) {
    return(x)
  }

  dim <- one_pad(x, y)

  if (xor(attr(x, "data.frame"), attr(y, "data.frame"))) {
    if (length(dim$x) > 2) {
      stop("Data frames must be 2d", call. = FALSE)
    }
    # Need to figure out how to forbid
    # recycle(data.frame(x = 1:5), matrix(nrow = 5, ncol = 2))
    # because the names are meaningless and it's no better than
    # recycle(1:5, matrix(nrow = 5, ncol = 2))
  }

  new_shape(map2_int(dim$x, dim$y, recycle_length))
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
