vecshape_coerce <- function(x, shape) {
  # Special cases
  if (is.null(x)) {
    return(NULL)
  } else if (is.null(shape)) {
    return(x)
  } else if (is.data.frame(x)) {
    # data frame recycling
    out <- vec_rep(x, shape[1])
    out <- out[rep_len(seq_along(out), shape[2])]
    return(out)
  } else if (vec_dims(x) == 1 && length(shape) == 1) {
    # vector recycling
    return(vec_rep(x, shape))
  } else if (length(x) == 1) {
    # scalar recycling
    return(array(x, dim = as.numeric(shape)))
  }

  dim(x) <- one_pad(vec_dim(x), shape)$x

  recycled <- dim(x) != shape

  indices <- rep(list(missing_arg()), length(shape))
  indices[recycled] <- map(shape[recycled], rep_len, x = 1L)

  eval_bare(expr(x[!!!indices, drop = FALSE]))
}
