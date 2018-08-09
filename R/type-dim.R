# The type of base vectors (lgl, int, dbl, chr, list) includes their
# dimensionality. This is needed to support matrix columns in data frames.
# Data frames don't support array cols, but we might as well be generic.

match_dim <- function(type, x, y) {
  structure(type, dim = common_dim(x, y))
}

common_dim <- function(x, y) {
  if (vec_dims(x) == 1L && vec_dims(y) == 1L) {
    return(NULL)
  } else if (xor(is.object(x), is.object(y))) {
    stop_incompatible_type(
      x, y,
      details = "S3 vectors can not have dimensions"
    )
  }

  dim <- zero_pad(vec_dim(x), vec_dim(y))
  out <- map2_int(dim$x, dim$y, recycle_dim)

  # Always treat "length" as free, so we can use values or prototypes
  out[[1]] <- 0L

  if (any(is.na(out))) {
    stop_incompatible_type(x, y, details = "Shapes are not compatible")
  }

  out
}

recycle_dim <- function(nx, ny) {
  if (nx == ny) {
    nx
  } else if (nx == 0L || ny == 0L) {
    max(nx, ny)
  } else {
    NA_integer_
  }
}

zero_pad <- function(x, y) {
  nx <- length(x)
  ny <- length(y)

  if (nx == ny) {
    list(x = x, y = y)
  } else if (nx < ny) {
    list(x = c(x, rep(0L, ny - nx)), y = y)
  } else {
    list(x = x, y = c(y, rep(0L, nx - ny)))
  }
}
