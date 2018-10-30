#' Number of observations
#'
#' `vec_size(x)` returns the size of a vector. This is distinct from the
#' [length()] of a vector because it generalises to the "number of observations"
#' for 2d structures, i.e. it's the number of rows in matrix or a data frame.
#' This definition has the important property that every column of a data frame
#' (even data frame and matrix columns) have the same size.
#' `vec_size_common(...)` returns the common size of multiple vectors.
#'
#' There is no vctrs helper that retrieves the number of columns: as this
#' is a property of the [type][vec_ptype()].
#'
#' `vec_size()` is equivalent to `NROW()` but has a name that is easier to
#' pronounce, and throws an error when passed non-vector inputs.
#'
#' @seealso [vec_slice()] for a variation of `[` compatible with `vec_size()`,
#'   and [vec_recycle()] to recycle vectors to common length.
#' @section Invariants:
#' * `vec_size(dataframe)` == `vec_size(dataframe[[i]])`
#' * `vec_size(matrix)` == `vec_size(matrix[, i, drop = FALSE])`
#' * `vec_size(vec_c(x, y))` == `vec_size(x)` + `vec_size(y)`
#'
#' @param x,... Vector inputs
#' @param .size If `NULL`, the default, the output size is determined by
#'   recycling the lengths of all elements of `...`. Alternatively, you can
#'   supply `.size` to force a known size.
#' @return An integer (or double for long vectors). Will throw an error
#'   if `x` is not a vector.
#'
#'   `vec_size_common()` will return `NULL` if all inputs are `NULL` or absent.
#' @export
#' @examples
#' vec_size(1:100)
#' vec_size(mtcars)
#' vec_size(array(dim = c(3, 5, 10)))
#'
#' vec_size(NULL)
#' # Because vec_size(vec_c(NULL, x)) ==
#' #   vec_size(NULL) + vec_size(x) ==
#' #   vec_size(x)
#'
#' vec_size_common(1:10, 1:10)
#' vec_size_common(1:10, 1)
#' vec_size_common(1:10, integer())
vec_size <- function(x) {
  .Call(vctrs_size, x)
}

#' @export
#' @rdname vec_size
vec_size_common <- function(..., .size = NULL) {
  if (!is.null(.size)) {
    return(.size)
  }

  args <- compact(list2(...))
  if (length(args) == 0)
    return(NULL)

  nobs <- map_int(args, vec_size)
  reduce(nobs, vec_size2)
}

vec_size2 <- function(nx, ny) {
  if (nx == ny) {
    nx
  } else if (nx == 0L || ny == 0L) {
    0L
  } else if (nx == 1L) {
    ny
  } else if (ny == 1L) {
    nx
  } else {
    stop("Incompatible lengths: ", nx, ", ", ny, call. = FALSE)
  }
}


# Slicing ----------------------------------------------------------------

#' Get or set observations in a vector
#'
#' This provides a common interface to extracting and modifying observations
#' for all vector types, regardless of dimensionality. It is an analog to `[`
#' that matches [vec_size()] instead of `length()`.
#'
#' @param x A vector
#' @param i An integer or character vector specifying the positions or
#'   names of the observations to get/set.
#' @param value Replacement values.
#' @export
#' @keywords internal
#' @examples
#' x <- sample(10)
#' x
#' vec_slice(x, 1:3)
#' vec_slice(x, 2L) <- 100
#' x
#'
#' vec_slice(mtcars, 1:3)
vec_slice <- function(x, i) {
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.integer(i) || is.character(i))

  if (is.null(x)) {
    NULL
  } else if (is.data.frame(x)) {
    # Much faster, and avoids creating rownames
    out <- lapply(x, `[`, i)
    vec_restore(out, x)
  } else if (is_vector(x)) {
    d <- vec_dims(x)
    if (d == 1) {
      if (is.object(x)) {
        x[i]
      } else {
        x[i, drop = FALSE]
      }
    } else if (d == 2) {
      x[i, , drop = FALSE]
    } else {
      miss_args <- rep(list(missing_arg()), d - 1)
      eval_bare(expr(x[i, !!!miss_args, drop = FALSE]))
    }
  } else {
    stop("`x` must be a vector", call. = FALSE)
  }
}

#' @export
#' @rdname vec_slice
`vec_slice<-` <- function(x, i, value) {
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.integer(i) || is.character(i))
  value <- vec_recycle(value, vec_size(i))

  if (is.null(x)) {
    NULL
  } else if (is_vector(x)) {
    d <- vec_dims(x)
    if (d == 1) {
      x[i] <- value
    } else if (d == 2) {
      x[i, ] <- value
    } else {
      miss_args <- rep(list(missing_arg()), d - 1)
      eval_bare(expr(x[i, !!!miss_args] <- value))
    }
  } else {
    stop("`x` must be a vector", call. = FALSE)
  }

  x
}

#' Create a missing vector
#'
#' @param x Template of missing vector
#' @param n Desired size of result
#' @export
#' @examples
#' vec_na(1:10, 3)
#' vec_na(Sys.Date(), 5)
#' vec_na(mtcars, 2)
vec_na <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}

# Names -------------------------------------------------------------------

vec_names <- function(x) {
  if (vec_dims(x) == 1) {
    names(x)
  } else if (is.data.frame(x)) {
    NULL
  } else {
    rownames(x)
  }
}

`vec_names<-` <- function(x, value) {
  if (vec_dims(x) == 1) {
    names(x) <- value
  } else {
    rownames(x) <- value
  }
  x
}
