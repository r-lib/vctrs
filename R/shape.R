# The dimensionality of an matrix/array is partition into two parts:
# * the first dimension = the number of observations
# * all other dimensions = the shape parameter of the type
# These helpers work with the shape parameter

shape_match <- function(type, x, y) {
  shape <- shape_common(x, y)
  if (length(shape) == 0L) {
    type
  } else {
    structure(type, dim = c(0L, shape))
  }
}

shape_common <- function(x, y) {
  shape <- dim2(shape(x), shape(y))
  map2_int(shape$x, shape$y, vec_size2)
}

shape_broadcast <- function(x, to) {
  if (is.null(x) || is.null(to))
    return(x)

  dim_x <- vec_dim(x)
  dim_to <- vec_dim(to)

  # Don't set dimensions for vectors
  if (length(dim_x) == 1L && length(dim_to) == 1L)
    return(x)

  if (length(dim_x) > length(dim_to))
    stop_incompatible_cast(x, to, details = "Can not decrease dimensions")

  dim_x <- dim2(dim_x, dim_to)$x
  dim_to[[1]] <- dim_x[[1]] # don't change number of observations
  ok <- dim_x == dim_to | dim_x == 1 | dim_to == 0
  if (any(!ok))
    stop_incompatible_cast(x, to, details = "Non-recyclable dimensions")

  recycle <- dim_x != dim_to

  indices <- rep(list(missing_arg()), length(dim_to))
  indices[recycle] <- map(dim_to[recycle], rep_len, x = 1L)

  dim(x) <- dim_x
  eval_bare(expr(x[!!!indices, drop = FALSE]))
}

# Helpers -----------------------------------------------------------------

shape <- function(x) {
  if (is.object(x)) {
    stop("Only bare vectors have shapes", call. = FALSE)
  }

  vec_dim(x)[-1]
}

dim2 <- function(x, y) {
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
