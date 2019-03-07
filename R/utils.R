indent <- function(x, n) {
  pad <- strrep(" ", n)
  map_chr(x, gsub, pattern = "(\n+)", replacement = paste0("\\1", pad))
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

has_inner_names <- function(x) {
  !all(map_lgl(map(x, vec_names), is.null))
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
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

#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%<-\%}} for details.
#' @importFrom zeallot %<-%
#' @export
#' @rdname unpack-assign
#' @name %<-%
#' @keywords internal
`%<-%`

has_unique_names <- function(x) {
  nms <- names(x)

  if (length(nms) != length(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}

compact <- function(x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

collapse_backtick <- function(x, last = " and ") {
  glue::glue_collapse(glue::backtick(x), ", ", last = last)
}
