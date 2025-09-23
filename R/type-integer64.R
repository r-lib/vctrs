#' @export
vec_proxy_equal.integer64 <- function(x, ...) {
  if (is.array(x)) {
    # Stopgap to convert arrays to data frames, then run them through
    # `vec_proxy_equal()` again, which will proxy each column
    x <- as_data_frame_from_array(x)
    x <- vec_proxy_equal(x)
    return(x)
  }

  integer64_proxy(x)
}

# Print -------------------------------------------------------------------

#' 64 bit integers
#'
#' A `integer64` is a 64 bits integer vector, implemented in the `bit64` package.
#'
#' These functions help the `integer64` class from `bit64` in to
#' the vctrs type system by providing coercion functions
#' and casting functions.
#'
#' @keywords internal
#' @rdname int64
#' @export
vec_ptype_full.integer64 <- function(x, ...) {
  "integer64"
}

#' @rdname int64
#' @export
vec_ptype_abbr.integer64 <- function(x, ...) {
  "int64"
}


# Coerce ------------------------------------------------------------------

#' @export
#' @rdname int64
#' @export vec_ptype2.integer64
#' @method vec_ptype2 integer64
vec_ptype2.integer64 <- function(x, y, ...) {
  UseMethod("vec_ptype2.integer64")
}

#' @method vec_ptype2.integer64 integer64
#' @export
vec_ptype2.integer64.integer64 <- function(x, y, ...) bit64::integer64()

#' @method vec_ptype2.integer64 integer
#' @export
vec_ptype2.integer64.integer <- function(x, y, ...) bit64::integer64()
#' @method vec_ptype2.integer integer64
#' @export
vec_ptype2.integer.integer64 <- function(x, y, ...) bit64::integer64()

#' @method vec_ptype2.integer64 logical
#' @export
vec_ptype2.integer64.logical <- function(x, y, ...) bit64::integer64()
#' @method vec_ptype2.logical integer64
#' @export
vec_ptype2.logical.integer64 <- function(x, y, ...) bit64::integer64()


# Cast --------------------------------------------------------------------

#' @export
#' @rdname int64
#' @export vec_cast.integer64
#' @method vec_cast integer64
vec_cast.integer64 <- function(x, to, ...) {
  UseMethod("vec_cast.integer64")
}

#' @export
#' @method vec_cast.integer64 integer64
vec_cast.integer64.integer64 <- function(x, to, ...) {
  x
}

#' @export
#' @method vec_cast.integer64 integer
vec_cast.integer64.integer <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.integer integer64
vec_cast.integer.integer64 <- function(x, to, ...) {
  as.integer(x)
}

#' @export
#' @method vec_cast.integer64 logical
vec_cast.integer64.logical <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.logical integer64
vec_cast.logical.integer64 <- function(x, to, ...) {
  as.logical(x)
}

#' @export
#' @method vec_cast.integer64 double
vec_cast.integer64.double <- function(x, to, ...) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.double integer64
vec_cast.double.integer64 <- function(x, to, ...) {
  as.double(x)
}

# ------------------------------------------------------------------------------

integer64_proxy <- function(x) {
  .Call(vctrs_integer64_proxy, x)
}
integer64_restore <- function(x) {
  .Call(vctrs_integer64_restore, x)
}

# ------------------------------------------------------------------------------

as_data_frame_from_array <- function(x) {
  # Alternative to `as.data.frame.array()` that always strips 1-D arrays
  # of their dimensions. Unlike `as.data.frame2()`, it doesn't unclass the
  # input, which means that each column retains its original class.
  # This function doesn't attempt to keep the names of `x` at all.

  dim <- dim(x)
  n_dim <- length(dim)

  if (n_dim == 1) {
    # Treat 1-D arrays as 1 column matrices
    dim(x) <- c(dim, 1L)
    n_dim <- 2L
  }

  n_row <- dim[[1L]]
  n_col <- prod(dim[-1L])
  n_col_seq <- seq_len(n_col)

  dim(x) <- c(n_row, n_col)

  out <- vector("list", n_col)
  names(out) <- as_unique_names(rep("", n_col), quiet = TRUE)

  for (i in n_col_seq) {
    out[[i]] <- x[, i, drop = TRUE]
  }

  new_data_frame(out, n = n_row)
}
