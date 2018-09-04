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
outer_names <- function(outer, names, n) {
  has_outer <- !is.null(outer) && !outer %in% c("", NA)
  if (!has_outer)
    return(names)

  has_inner <- !is.null(names)
  if (has_inner) {
    paste0(outer, "..", names)
  } else {
    if (n == 1) {
      outer
    } else {
      paste0(outer, seq_len(n))
    }
  }
}

hash_label <- function(x, length = 5) {
  if (length(x) == 0) {
    ""
  } else {
    substr(digest::digest(x), 1, length)
  }
}

cat_line <- function(...) {
  cat(..., "\n", sep = "")
}


set_partition <- function(x, y) {
  list(
    both = intersect(x, y),
    only_x = setdiff(x, y),
    only_y = setdiff(y, x)
  )
}

all_equal <- function(x) all(x == x[[1]])

inline_list <- function(title, x, width = getOption("width"), quote = "") {
  label_width <- width - nchar(title)
  x <- glue::glue_collapse(
    encodeString(x, quote = quote),
    sep = ", ",
    width = label_width
  )
  paste0(title, x)
}

# other than names
has_attrs <- function(x) {
  attr <- attributes(x)
  if (is.null(attr))
    return(FALSE)

  !identical(names(attr), "names")
}
