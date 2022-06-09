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
#' These arguments are handled by the generic and not passed to methods:
#' * `prefix_named`
#' * `suffix_shape`
#'
#' @param x A vector.
#' @param prefix_named If `TRUE`, add a prefix for named vectors.
#' @param suffix_shape If `TRUE` (the default), append the shape of
#'   the vector.
#' @inheritParams rlang::args_dots_empty
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
  check_dots_empty0(...)

  # Data frames dispatch to the `data.frame` method by default to get
  # the inner types format
  method <- s3_method_specific(
    x,
    "vec_ptype_full",
    ns = "vctrs",
    df_default = TRUE
  )
  return(method(x, ...))

  UseMethod("vec_ptype_full")
}

#' @export
#' @rdname vec_ptype_full
vec_ptype_abbr <- function(x, ..., prefix_named = FALSE, suffix_shape = TRUE) {
  check_dots_empty0(...)

  method <- s3_method_specific(x, "vec_ptype_abbr", ns = "vctrs")
  abbr <- method(x, ...)

  named <- if ((prefix_named || is_bare_list(x)) && !is.null(vec_names(x))) "named "
  shape <- if (suffix_shape) vec_ptype_shape(x)
  abbr <- paste0(named, abbr, shape)

  return(abbr)
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
vec_ptype_abbr.default <- function(x, ...) {
  if (is.data.frame(x)) {
    type <- class(x)[[1]]
  } else if (is.object(x)) {
    type <- vec_ptype_full(x)
  } else if (is_vector(x)) {
    type <- switch(typeof(x),
      list = "list",
      logical = "lgl",
      integer = "int",
      double = "dbl",
      character = "chr",
      complex = "cpl",
      list = "list",
      expression = "expr",
      raw = "raw",
      typeof(x)
    )
  } else {
    abort("Not a vector.")
  }
  unname(abbreviate(type, 8))
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
