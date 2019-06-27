# The dimensionality of an matrix/array is partition into two parts:
# * the first dimension = the number of observations
# * all other dimensions = the shape parameter of the type
# These helpers work with the shape parameter

new_shape <- function(type, shape = NULL) {
  if (length(shape) == 0L) {
    type
  } else {
    structure(type, dim = c(0L, shape))
  }
}

shape_match <- function(type, x, y) {
  shape <- shape_common(x, y)
  new_shape(type, shape)
}

shape_common <- function(x, y) {
  shape <- n_dim2(shape(x), shape(y))
  map2_int(shape$x, shape$y, axis2)
}

axis2 <- function(nx, ny) {
  if (nx == ny) {
    nx
  } else if (nx == 1L) {
    ny
  } else if (ny == 1L) {
    nx
  } else {
    abort(paste0("Incompatible lengths: ", nx, ", ", ny, "."))
  }
}

shape_broadcast <- function(x, to) {
  if (is.null(x) || is.null(to)) {
    return(x)
  }

  dim_x <- vec_dim(x)
  dim_to <- vec_dim(to)

  # Don't set dimensions for vectors
  if (length(dim_x) == 1L && length(dim_to) == 1L) {
    return(x)
  }

  if (length(dim_x) > length(dim_to)) {
    stop_incompatible_cast(x, to, details = "Can not decrease dimensions")
  }

  dim_x <- n_dim2(dim_x, dim_to)$x
  dim_to[[1]] <- dim_x[[1]] # don't change number of observations
  ok <- dim_x == dim_to | dim_x == 1
  if (any(!ok)) {
    stop_incompatible_cast(x, to, details = "Non-recyclable dimensions")
  }

  # Increase dimensionality if required
  if (vec_dim_n(x) != length(dim_x)) {
    dim(x) <- dim_x
  }

  recycle <- dim_x != dim_to

  # Avoid expensive subset
  if (all(!recycle)) {
    return(x)
  }

  indices <- rep(list(missing_arg()), length(dim_to))
  indices[recycle] <- map(dim_to[recycle], rep_len, x = 1L)

  eval_bare(expr(x[!!!indices, drop = FALSE]))
}

# Helpers -----------------------------------------------------------------

shape <- function(x) {
  if (is.object(x)) {
    abort("Only bare vectors have shapes.")
  }

  vec_dim(x)[-1]
}

n_dim2 <- function(x, y) {
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
