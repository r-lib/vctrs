#' Number of observations
#'
#' `vec_obs()` returns the number of observations in an object. This is
#' the length of a 1d vector, or the number of rows in a matrix or data frame.
#' There is no vctrs helper that retrieves the number of columns: as this
#' is a property of the [type][vec_ptype()].
#'
#' `vec_obs()` is equivalent to `NROW()` but has a name that is easier to
#' pronounce, and throws an error when passed non-vector inputs.
#'
#' @seealso [vec_slice()] for a variation of `[` compatible with `vec_obs()`.
#' @section Invariants:
#' * `vec_obs(dataframe)` == `vec_obs(dataframe[[i]])`
#' * `vec_obs(matrix)` == `vec_obs(matrix[, i, drop = FALSE])`
#' * `vec_obs(vec_c(x, y))` == `vec_obs(x)` + `vec_obs(y)`
#' @param x A vector
#' @return An integer (or double for long vectors). Will throw an error
#'   if `x` is not a vector.
#' @export
#' @examples
#' vec_obs(1:100)
#' vec_obs(mtcars)
#' vec_obs(array(dim = c(3, 5, 10)))
#'
#' vec_obs(NULL)
#' # Because vec_obs(vec_c(NULL, x)) ==
#' #   vec_obs(NULL) + vec_obs(x) ==
#' #   vec_obs(x)
vec_obs <- function(x) {
  .Call(vctrs_length, x)
}

# Slicing ----------------------------------------------------------------

#' Get or set observations in a vector
#'
#' This provides a common interface to extracting and modifying observations
#' for all vector types, regardless of dimensionality. It is an analog to `[`
#' that matches [vec_obs()] instead of `length()`.
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
  stopifnot(is.integer(i) || is.character(i))
  stopifnot(length(i) == length(value))

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

vec_na <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}

vec_rep <- function(x, n) {
  id <- rep_len(seq_len(vec_obs(x)), n)
  vec_slice(x, id)
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
