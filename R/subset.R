vec_subset <- function(x, i) {
  if (is.null(x)) return(NULL)

  stopifnot(is_vector(x))
  stopifnot(is.integer(i) || is.character(i))

  d <- vec_dims(x)
  if (d == 1) {
    x[i, drop = FALSE]
  } else if (d == 2) {
    out <- x[i, , drop = FALSE]

    if (is.data.frame(x) && .row_names_info(x) < 0) {
      row.names(out) <- NULL
    }
    out

  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    eval_bare(expr(x[i, !!!miss_args, drop = FALSE]))
  }
}

vec_na <- function(x, n = 1L) {
  vec_subset(x, rep_len(NA_integer_, n))
}

vec_rep <- function(x, n) {
  id <- rep_len(seq_len(vec_length(x)), n)
  vec_subset(x, id)
}
