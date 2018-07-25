vec_subset <- function(x, i) {
  stopifnot(is_vector(x))
  stopifnot(is.integer(i) || is.character(i))

  d <- vec_dims(x)
  if (d == 1) {
    x[i, drop = FALSE]
  } else if (d == 2) {
    x[i, , drop = FALSE]
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

