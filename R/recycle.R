recycle2 <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(list(x = x, y = y))
  }

  nx <- length(x)
  ny <- length(y)

  if (nx == ny) {
    list(x = x, y = y)
  } else if (nx == 1L || ny == 1L) {
    if (nx > ny) {
      list(x = x, y = rep.int(y, nx))
    } else {
      list(x = rep.int(x, ny), y = y)
    }
  } else if (nx == 0L || ny == 0L) {
    list(x = x[0L], y = y[0L])
  } else  {
    stop("Incompatible lengths: ", nx, ", ", ny, call. = FALSE)
  }
}
