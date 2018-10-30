#' Find the prototype of a set of vectors
#'
#' `vec_type()` finds the prototype of a single vector.
#' `vec_type_common()` finds the common type of multiple vectors.
#' `vec_ptype()` nicely prints the common type of any number of
#' inputs, and is designed for interative exploration.
#'
#' `vec_type_common()` first finds the prototype of each input, then
#' finds the common type using [vec_type2()] and [Reduce()].
#'
#' @section Prototype:
#' A prototype is [size](vec_size) 0 vector containing attributes, but no
#' data. Generally, this is just `vec_slice(x, 0L)`, but some inputs
#' require special handling.
#'
#' For example, the prototype of logical vectors that only contain missing
#' values is the special [unspecified] type, which can be coerced to any
#' other 1d type. This allows bare `NA`s to represent missing values for
#' any 1d vector type.
#'
#' @param ...,x Vectors inputs
#' @param .ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `...`.
#'
#'   Alternatively, you can supply `.ptype` to give the output known type.
#'   If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this value:
#'   this is a convenient way to make production code demand fixed types.
#' @return `vec_type()` and `vec_type_common()` return a prototype
#'   (a size-0 vector)
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

#' @export
#' @rdname vec_type
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

#' @export
#' @rdname vec_type
vec_ptype <- function(..., .ptype = NULL) {
  type <- vec_type_common(..., .ptype = .ptype)

  cat_line("prototype: ", vec_ptype_full(type))
  invisible()
}

