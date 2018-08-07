indent <- function(x, n) {
  if (length(x) == 0)
    return(character())

  pad <- strrep(" ", n)

  out <- Map(gsub, "\n", paste0("\n", pad), x)
  unlist(out, use.names = FALSE)
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
outer_names <- function(x, outer) {
  has_outer <- !is.null(outer) && !outer %in% c("", NA)
  if (!has_outer)
    return(names(x))

  has_inner <- !is.null(names(x))
  if (has_inner) {
    paste0(outer, "..", names(x))
  } else {
    if (length(x) == 1) {
      outer
    } else {
      paste0(outer, seq_along(x))
    }
  }
}

