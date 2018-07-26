recycle2 <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(list(x = x, y = y))
  }

  nx <- vec_length(x)
  ny <- vec_length(y)

  if (nx == ny) {
    list(x = x, y = y)
  } else if (nx == 1L || ny == 1L) {
    if (nx > ny) {
      list(x = x, y = vec_rep(y, nx))
    } else {
      list(x = vec_rep(x, ny), y = y)
    }
  } else if (nx == 0L || ny == 0L) {
    list(x = vec_subset(x, 0L), y = vec_subset(y, 0L))
  } else {
    stop("Incompatible lengths: ", nx, ", ", ny, call. = FALSE)
  }
}
