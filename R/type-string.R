#' Vector type as a string
#'
#' `vec_ptype_full()` displays the full type of the vector. `vec_ptype_abbr()`
#' provides an abbreviated summary suitable for use in a column heading.
#'
#' @section S3 dispatch:
#' The default method for `vec_ptype_full()` uses the first element of the
#' class vector. Override this method if your class has parameters that should
#' be prominently displayed.
#'
#' The default method for `vec_ptype_abbr()` [abbreviate()]s `vec_ptype_full()`
#' to 8 characters. You should almost always override, aiming for 4-6
#' characters where possible.
#'
#' @param x A vector.
#' @keywords internal
#' @return A string.
#' @export
#' @examples
#' cat(vec_ptype_full(1:10))
#' cat(vec_ptype_full(iris))
#'
#' cat(vec_ptype_abbr(1:10))
vec_ptype_full <- function(x) {
  UseMethod("vec_ptype_full")
}

#' @export
#' @rdname vec_ptype_full
vec_ptype_abbr <- function(x) {
  UseMethod("vec_ptype_abbr")
}

vec_ptype_full.NULL <- function(x) "NULL"
vec_ptype_abbr.NULL <- function(x) "NULL"

# Default: base types and fallback for S3/S4 ------------------------------

#' @export
vec_ptype_full.default <- function(x) {
  if (is.object(x)) {
    class(x)[[1]]
  } else if (is_vector(x)) {
    paste0(typeof(x), vec_ptype_shape(x))
  } else {
    stop("Not a vector", call. = FALSE)
  }
}

#' @export
vec_ptype_abbr.default <- function(x) {
  if (is.object(x)) {
    unname(abbreviate(vec_ptype_full(x), 8))
  } else if (is_vector(x)) {
    abbr <- switch(typeof(x),
      logical = "lgl",
      integer = "int",
      double = "dbl",
      character = "chr",
      complex = "cplx",
      list = "list",
      expression = "expr"
    )
    paste0(abbr, vec_ptype_shape(x))
  } else {
    stop("Not a vector", call. = FALSE)
  }
}

# Date/times --------------------------------------------------------------

#' @export
vec_ptype_full.Date <- function(x) {
  "date"
}

#' @export
vec_ptype_abbr.Date <- function(x) {
  "date"
}

#' @export
vec_ptype_full.POSIXt <- function(x) {
  paste0("datetime<", attr(x, "tzone") %||% "local", ">")
}

#' @export
vec_ptype_abbr.POSIXct <- function(x) {
  "dttm"
}

#' @export
vec_ptype_full.difftime <- function(x) {
  paste0("time<", attr(x, "units"), ">")
}

#' @export
vec_ptype_abbr.difftime <- function(x) {
  "time"
}

# Factors -----------------------------------------------------------------

#' @export
vec_ptype_abbr.ordered <- function(x) {
  "ord"
}
#' @export
vec_ptype_abbr.factor <- function(x) {
  "fctr"
}

#' @export
vec_ptype_full.ordered <- function(x) {
  paste0("ordered<", hash(levels(x)), ">")
}

#' @export
vec_ptype_full.factor <- function(x) {
  paste0("factor<", hash(levels(x)), ">")
}

# Data frame --------------------------------------------------------------

#' @export
vec_ptype_full.data.frame <- function(x) {
  if (length(x) == 0) {
    return(paste0(class(x)[[1]], "<>"))
  } else if (length(x) == 1) {
    return(paste0(class(x)[[1]], "<", names(x), ":", vec_ptype_full(x[[1]]), ">"))
  }

  # Needs to handle recursion with indenting
  types <- map_chr(x, vec_ptype_full)
  needs_indent <- grepl("\n", types)
  types[needs_indent] <- map(types[needs_indent], function(x) indent(paste0("\n", x), 4))

  names <- paste0("  ", format(names(x)))

  paste0(
    .Class[[1]], "<\n",
    paste0(names, ": ", types, collapse = "\n"),
    "\n>"
  )
}

#' @export
vec_ptype_abbr.data.frame <- function(x) {
  paste0("df", vec_ptype_shape(x))
}

# AsIs --------------------------------------------------------------------

#' @export
vec_ptype_full.AsIs <- function(x) {
  paste0("I<", NextMethod(), ">")
}

#' @export
vec_ptype_abbr.AsIs <- function(x) {
  paste0("I<", NextMethod(), ">")
}

# Helpers -----------------------------------------------------------------

# In the type specification of bare vectors, a zero means free specification
vec_ptype_shape <- function(x) {
  if (vec_dims(x) == 1) {
    ""
  } else {
    dim <- vec_dim(x)
    paste0("[", paste(ifelse(dim == 0L, "", dim), collapse = ","), "]")
  }
}
