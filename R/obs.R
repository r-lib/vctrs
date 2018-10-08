#' Number of observations
#'
#' `vec_obs()` returns the number of observations in an object. This is
#' the length of a 1d vector, or the number of rows in a matrix or data frame.
#'
#' @param x A vector
#' @export
#' @examples
#' vec_obs(1:100)
#' vec_obs(mtcars)
#' vec_obs(array(dim = c(3, 5, 10)))
vec_obs <- function(x) {
  .Call(vctrs_length, x)
}

# Rename to vec_slice before exporting
vec_subset <- function(x, i) {
  if (is.null(x)) {
    NULL
  } else if (is.data.frame(x)) {
    # Much faster, and avoids creating rownames
    out <- lapply(x, `[`, i)
    vec_restore(out, x)
  } else if (is_vector(x)) {
    d <- vec_dims(x)
    if (d == 1) {
      x[i, drop = FALSE]
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

`vec_subset<-` <- function(x, i, value) {
  if (is.null(x)) return(NULL)

  stopifnot(is_vector(x))
  stopifnot(is.integer(i) || is.character(i))

  d <- vec_dims(x)
  if (d == 1) {
    x[i] <- value
  } else if (d == 2) {
    x[i, ] <- value
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    eval_bare(expr(x[i, !!!miss_args] <- value))
  }
  x
}

vec_na <- function(x, n = 1L) {
  vec_subset(x, rep_len(NA_integer_, n))
}

vec_rep <- function(x, n) {
  id <- rep_len(seq_len(vec_obs(x)), n)
  vec_subset(x, id)
}

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
