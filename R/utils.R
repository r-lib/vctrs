hash <- function(x) {
  substr(digest::digest(x), 1, 5)
}

indent <- function(x, n) {
  if (length(x) == 0)
    return(character())

  pad <- strrep(" ", n)

  out <- Map(gsub, "\n", paste0("\n", pad), x)
  unlist(out, use.names = FALSE)
}

set_compare <- function(x, y) {
  if (identical(x, y) || setequal(x, y)) {
    "equal"
  } else if (length(setdiff(x, y)) == 0) {
    "x_in_y"
  } else if (length(setdiff(y, x)) == 0) {
    "y_in_x"
  } else {
    "notequal"
  }
}

ones <- function(...) {
  array(1, dim = c(...))
}
