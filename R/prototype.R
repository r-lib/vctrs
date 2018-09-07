#' Find the prototype of a set of vectors
#'
#' This function is aimed at developers, but may also be useful for users who
#' want to explore the type system interactively. Use this function to find the
#' common type from a set of inputs.
#'
#' This function works by finding the prototype (a zero-length subset) of each
#' input, then using [Reduce()] and [vec_type2()] to find the common class.
#' `NULL`s and logical vectors that consist only of `NA` (including those inside
#' data frames) are converted to the special [unknown] type. This is needed
#' because bare `NA`s should be automatically coercible to every other class.
#'
#' @param ... Vectors inputs
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
  if (!is.null(.ptype)) {
    ptype <- as_vec_ptype(.ptype)
  } else {
    if (isTRUE(getOption("vctrs.no_guessing"))) {
      stop("strict mode is activated; you must supply .ptype", call. = FALSE)
    }

    args <- list2(...)
    ptypes <- map(args, as_vec_ptype)
    ptype <- reduce(ptypes, vec_type2, .init = unknown())
  }

  new_vec_ptype(ptype)
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

as_vec_ptype <- function(x) {
  UseMethod("as_vec_ptype")
}

#' @export
as_vec_ptype.vec_ptype <- function(x) {
  x[[1]]
}

#' @export
as_vec_ptype.default <- function(x) {
  vec_subset(x, 0L)
}

#' @export
as_vec_ptype.NULL <- function(x) {
  unknown()
}

#' @export
as_vec_ptype.logical <- function(x) {
  if (is_nullish(x)) {
    unknown()
  } else {
    vec_subset(x, 0L)
  }
}

#' @export
as_vec_ptype.data.frame <- function(x) {
  null_cols <- unname(map_lgl(x, is_nullish))

  cols <- as.list(x)
  cols[null_cols] <- rep.int(list(unknown()), sum(null_cols))
  cols[!null_cols] <- map(cols[!null_cols], as_vec_ptype)

  new_data_frame(cols, 0L, setdiff(class(x), "data.frame"))
}

