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

vec_na <- function(x) {
  if (is_list(x) && vec_dims(x) == 1L) {
    # list()[NA_integer_] returns list(NULL)
    NULL
  } else {
    vec_subset(x, NA_integer_)
  }
}


vec_rep <- function(x, n) {
  id <- rep_len(seq_len(vec_length(x)), n)
  vec_subset(x, id)
}
