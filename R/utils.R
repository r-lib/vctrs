vec_na <- function(x) {
  if (is.list(x)) {
    # list()[NA_integer_] returns list(NULL)
    NULL
  } else {
    vec_subset(x, NA_integer_)
  }
}

