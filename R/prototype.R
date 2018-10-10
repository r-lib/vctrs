#' Find the prototype of a set of vectors
#'
#' `vec_type_common()` finds the common type from a set of vectors.
#' `vec_ptype()` is designed for interative exploration: it wraps
#' `vec_type_common()` with a custom class that gives a nice output.
#'
#' This function works by finding the prototype (a zero-length subset) of each
#' input, then using [Reduce()] and [vec_type2()] to find the common class.
#' Logical vectors that consist only of `NA` are converted to the special
#' [unspecified] type. This is needed because bare `NA`s should be
#' automatically coercible to any 1d vector.
#'
#' @param ...,x Vectors inputs
#' @param .ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `...`.
#'
#'   Alternatively, you can supply `.ptype` to give the output known type.
#'   If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this value:
#'   this is a convenient way to make production code demand fixed types.
#' @return A prototype
#' @export
#' @examples
#' # Unknown types ------------------------------------------
#' vec_ptype()
#' vec_ptype(NA)
#' vec_ptype(NULL)
#'
#' # Vectors ------------------------------------------------
#' vec_ptype(1:10)
#' vec_ptype(letters)
#' vec_ptype(TRUE)
#'
#' vec_ptype(Sys.Date())
#' vec_ptype(Sys.time())
#' vec_ptype(factor("a"))
#' vec_ptype(ordered("a"))
#'
#' # Matrices -----------------------------------------------
#' # The prototype of a matrix includes the number of columns
#' vec_ptype(array(1, dim = c(1, 2)))
#' vec_ptype(array("x", dim = c(1, 2)))
#'
#' # Data frames --------------------------------------------
#' # The prototype of a data frame includes the prototype of
#' # every column
#' vec_ptype(iris)
#'
#' # The prototype of multiple data frames includes the prototype
#' # of every column that in any data frame
#' vec_ptype(
#'   data.frame(x = TRUE),
#'   data.frame(y = 2),
#'   data.frame(z = "a")
#' )
vec_ptype <- function(..., .ptype = NULL) {
  type <- vec_type_common(..., .ptype = .ptype)
  new_vec_ptype(type)
}

#' @export
#' @rdname vec_ptype
vec_type_common <- function(..., .ptype = NULL) {
  if (!is_partial(.ptype)) {
    return(vec_type(.ptype))
  }

  if (isTRUE(getOption("vctrs.no_guessing"))) {
    stop("strict mode is activated; you must supply complete .ptype", call. = FALSE)
  }

  args <- compact(list2(.ptype, ...))
  if (length(args) == 0) {
    ptype <- NULL
  } else if (length(args) == 1) {
    ptype <- vec_type(args[[1]])
  } else {
    ptypes <- map(args, vec_type)
    ptype <- reduce(ptypes, vec_type2)
  }

  vec_type_finalise(ptype)
}


new_vec_ptype <- function(ptype) {
  structure(
    list(ptype),
    class = "vec_ptype"
  )
}

#' @export
print.vec_ptype <- function(x, ...) {
  cat("prototype: ", format(x), "\n", sep = "")
  invisible(x)
}

#' @export
format.vec_ptype <- function(x, ...) {
  vec_ptype_full(x[[1]])
}
#' @export
as.character.vec_ptype <- format.vec_ptype

#' @export
#' @rdname vec_ptype
vec_type <- function(x) {
  UseMethod("vec_type")
}

#' @export
vec_type.default <- function(x) {
  if (is_vector(x)) {
    vec_slice(x, 0L)
  } else {
    stop("`x` is not a vector", call. = FALSE)
  }
}

#' @export
vec_type.NULL <- function(x) {
  NULL
}

#' @export
vec_type.logical <- function(x) {
  if (is_unspecified(x)) {
    unspecified()
  } else {
    vec_slice(x, 0L)
  }
}

#' @export
vec_type.data.frame <- function(x) {
  cols <- map(x, vec_type)
  vec_restore(cols, x)
}

