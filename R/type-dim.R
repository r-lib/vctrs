# The type of base vectors (lgl, int, dbl, chr, list) includes their
# dimensionality. This is needed to support matrix columns in data frames.
# Data frames don't support array cols, but we might as well be generic.

dim_match <- function(type, x, y) {
  structure(type, dim = dim_common(x, y))
}

dim_common <- function(x, y) {
  if (vec_dims(x) == 1L && vec_dims(y) == 1L) {
    return(NULL)
  }

  if (xor(is.object(x), is.object(y))) {
    stop_incompatible_type(
      x, y,
      details = "S3 vectors can not have dimensions"
    )
  } else if (vec_dims(x) != vec_dims(y)) {
    stop_incompatible_type(x, y, details = "Dimensionality must be equal")
  }

  dim_x <- vec_dim(x)
  dim_y <- vec_dim(y)

  max_dim <- pmax(dim_x, dim_y)
  out <- ifelse(dim_x == 0 | dim_y == 0 | dim_x == dim_y, max_dim, NA)

  # Always treat "length" as free, so we can use values or prototypes
  out[[1]] <- 0L

  if (any(is.na(out))) {
    stop_incompatible_type(x, y, details = "Shapes are not compatible")
  }

  out
}

shape_recycle <- function(x, to) {
  if (is.null(x) || is.null(to) || is.object(to))
    return(x)

  # Can always cast down to a vector
  if (identical(vec_dim(to), 0L)) {
    if (vec_dims(x) == 1L) {
      return(x)
    } else {
      return(structure(x, dim = NULL))
    }
  }

  if (vec_dims(x) != vec_dims(to)) {
    stop_incompatible_cast(x, to, details = "Dimensionality must be equal")
  }

  # Recycle along any non-zero components that need it
  to_recycle <- dim(to) != 0 & dim(x) != dim(to)

  bad_length <- to_recycle & dim(x) != 1L
  if (any(bad_length)) {
    stop_incompatible_cast(
      x, to,
      details = "Can only recycle along dimensions of size 1"
    )
  }

  indices <- rep(list(missing_arg()), vec_dims(to))
  indices[to_recycle] <- map(vec_dim(to)[to_recycle], rep_len, x = 1L)

  eval_bare(expr(x[!!!indices, drop = FALSE]))
}
