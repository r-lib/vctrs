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
  id <- rep_len(seq_len(vec_length(x)), n)
  vec_subset(x, id)
}
