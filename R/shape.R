# The dimensionality of an matrix/array is partition into two parts:
# * the first dimension = the number of observations
# * all other dimensions = the shape parameter of the type
# These helpers work with the shape parameter

new_shape <- function(type, shape = integer()) {
  structure(type, dim = c(0L, shape))
}

vec_shaped_ptype  <- function(ptype, x, y, ..., x_arg = "", y_arg = "") {
  check_dots_empty0(...)
  .Call(vctrs_shaped_ptype, ptype, x, y, x_arg, y_arg)
}

vec_shape2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  check_dots_empty0(...)
  .Call(vctrs_shape2, x, y, x_arg, y_arg)
}

# Should take same signature as `vec_cast()`
shape_broadcast <- function(x,
                            to,
                            ...,
                            x_arg,
                            to_arg,
                            call = caller_env()) {
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
    stop_incompatible_cast(
      x,
      to,
      details = "Cannot decrease dimensions.",
      x_arg = x_arg,
      to_arg = to_arg,
      call = call
    )
  }

  dim_x <- n_dim2(dim_x, dim_to)$x
  dim_to[[1]] <- dim_x[[1]] # don't change number of observations
  ok <- dim_x == dim_to | dim_x == 1
  if (any(!ok)) {
    stop_incompatible_cast(
      x,
      to,
      details = "Non-recyclable dimensions.",
      x_arg = x_arg,
      to_arg = to_arg,
      call = call
    )
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
