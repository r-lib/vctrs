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

vec_coerce_bare <- function(x, type) {
  # Unexported wrapper around Rf_coerceVector()
  coerce <- env_get(ns_env("rlang"), "vec_coerce")
  coerce(x, type)
}


# Matches the semantics of c() - based on experimenting with the output
# of c(), not reading the source code.
name_outer <- function(x, outer) {
  has_outer <- !outer %in% c("", NA)
  if (!has_outer)
    return(x)

  has_inner <- !is.null(names(x))
  if (has_inner) {
    names(x) <- paste0(outer, ".", names(x))
  } else {
    if (length(x) == 1) {
      names(x) <- outer
    } else {
      names(x) <- paste0(outer, seq_along(x))
    }
  }

  x
}
