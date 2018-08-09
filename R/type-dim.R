# The type of base vectors (lgl, int, dbl, chr, list) includes their
# dimensionality. This is needed to support matrix columns in data frames.
# Data frames don't support array cols, but we might as well be generic.

dim_match <- function(type, x, y) {
  structure(type, dim = dim_common(x, y))
}

dim_common <- function(x, y) {
  if (vec_dims(x) == 1L && vec_dims(y) == 1L) {
    return(NULL)
  } else if (xor(is.object(x), is.object(y))) {
    stop_incompatible_type(
      x, y,
      details = "S3 vectors can not have dimensions"
    )
  }

  dim <- zero_pad(vec_dim(x), vec_dim(y))
  max_dim <- pmax(dim$x, dim$y)
  out <- ifelse(dim$x == 0 | dim$y == 0 | dim$x == dim$y, max_dim, NA)

  # Always treat "length" as free, so we can use values or prototypes
  out[[1]] <- 0L

  if (any(is.na(out))) {
    stop_incompatible_type(x, y, details = "Shapes are not compatible")
  }

  out
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
