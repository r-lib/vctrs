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
#' @param prefix_named Add a prefix for named vectors.
#' @param shape Include the shape of the vector.
#' @inheritParams ellipsis::dots_empty
#'
#' @keywords internal
#' @return A string.
#' @export
#' @examples
#' cat(vec_ptype_full(1:10))
#' cat(vec_ptype_full(iris))
#'
#' cat(vec_ptype_abbr(1:10))
vec_ptype_full <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  UseMethod("vec_ptype_full")
}

#' @export
#' @rdname vec_ptype_full
vec_ptype_abbr <- function(x, ..., prefix_named = FALSE, shape = TRUE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  UseMethod("vec_ptype_abbr")
}

#' @export
vec_ptype_full.NULL <- function(x, ...) "NULL"

#' @export
vec_ptype_abbr.NULL <- function(x, ...) "NULL"

# Default: base types and fallback for S3/S4 ------------------------------

#' @export
vec_ptype_full.default <- function(x, ...) {
  if (is.object(x)) {
    class(x)[[1]]
  } else if (is_vector(x)) {
    paste0(typeof(x), vec_ptype_shape(x))
  } else {
    abort("Not a vector.")
  }
}

#' @export
vec_ptype_abbr.default <- function(x, ..., prefix_named = FALSE, shape = TRUE) {
  if (is.object(x)) {
    unname(abbreviate(vec_ptype_full(x), 8))
  } else if (is_list(x)) {
    named <- is_character(names(x))
    # Always print "named" even if !prefix_named, for compatibility
    paste0(if (named) "named ", "list", vec_ptype_shape(x))
  } else if (is_vector(x)) {
    abbr <- switch(typeof(x),
      logical = "lgl",
      integer = "int",
      double = "dbl",
      character = "chr",
      complex = "cpl",
      list = "list",
      expression = "expr",
      raw = "raw",
      abbreviate(typeof(x))
    )
    paste0(
      if (prefix_named && !is.null(x) && !is.null(vec_names(x))) "named ",
      abbr,
      if (shape) vec_ptype_shape(x)
    )
  } else {
    abort("Not a vector.")
  }
}

# Helpers -----------------------------------------------------------------

vec_ptype_shape <- function(x) {
  dim <- dim2(x)
  if (length(dim) == 1) {
    ""
  } else {
    paste0("[,", paste(dim[-1], collapse = ","), "]")
  }
}
