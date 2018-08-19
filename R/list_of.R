#' Construct "list_of" objects
#'
#' A `list_of` object is a list where each element has the same type.
#' Modifying the list with `$`, `[`, and `[[` preserves the constraint
#' by coercing all input items.
#'
#' @inheritParams vec_c
#' @param x For `as_list_of()`, a vector to be coerced to list_of.
#' @param y,to Arguments to `vec_type2()` and `vec_cast()`.
#' @export
#' @examples
#' x <- list_of(1:3, 5:6, 10:15)
#' tibble::tibble(x = x)
#'
#' vec_c(list_of(1, 2), list_of(FALSE, TRUE))
list_of <- function(..., .ptype = NULL) {
  args <- list2(...)

  ptype <- vec_ptype(!!!args, .ptype = .ptype)[[1]]
  if (is_unknown(ptype)) {
    stop("Could not find common type for elements of `x`", call. = FALSE)
  }

  x <- map(args, vec_cast, to = ptype)
  new_list_of(x, ptype)
}

#' @export
#' @rdname list_of
as_list_of <- function(x, ...) {
  UseMethod("as_list_of")
}

#' @export
as_list_of.list_of <- function(x, .ptype = NULL, ...) {
  if (!is.null(.ptype)) {
    list_of(!!!x, .ptype = .ptype)
  } else {
    x
  }
}

#' @export
as_list_of.list <- function(x, ..., .ptype = NULL) {
  list_of(!!!x, .ptype = .ptype)
}

#' @export
#' @rdname list_of
new_list_of <- function(x, .ptype) {
  stopifnot(is.list(x))
  stopifnot(vec_length(.ptype) == 0)

  structure(
    x,
    ptype = .ptype,
    class = "list_of"
  )
}

#' @export
#' @rdname list_of
is_list_of <- function(x) {
  inherits(x, "list_of")
}

# registered .onLoad
type_sum.list_of <- function(x) {
  paste0("list<", tibble::type_sum(attr(x, "ptype")), ">")
}

#' @export
vec_ptype_full.list_of <- function(x) {
  param <- vec_ptype_full(attr(x, "ptype"))
  if (grepl("\n", param)) {
    param <- paste0(indent(paste0("\n", param), 2), "\n")
  }

  paste0("list_of<", param, ">")
}

#' @export
print.list_of <- function(x, ...) {
  cat(format(vec_ptype(x)), "\n", sep = "")

  # Expensive: need to find a better way
  attr(x, "ptype") <- NULL
  class(x) <- NULL

  print(x)
}

#' @export
as.list.list_of <- function(x, ...) {
  attr(x, "ptype") <- NULL
  attr(x, "class") <- NULL
  x
}

#' @export
`[.list_of` <- function(x, ...) {
  new_list_of(NextMethod(), attr(x, "ptype"))
}

#' @export
`[<-.list_of` <- function(x, i, value) {
  value <- map(value, vec_cast, attr(x, "ptype"))
  NextMethod()
}

#' @export
`[[<-.list_of` <- function(x, i, value) {
  value <- vec_cast(value, attr(x, "ptype"))
  NextMethod()
}

#' @export
`$<-.list_of` <- function(x, i, value) {
  value <- vec_cast(value, attr(x, "ptype"))
  NextMethod()
}

# Type system -------------------------------------------------------------

#' @rdname list_of
#' @export vec_type2.list_of
#' @method vec_type2 list_of
#' @export
vec_type2.list_of <- function(x, y) UseMethod("vec_type2.list_of", y)
#' @method vec_type2.list_of unknown
#' @export
vec_type2.list_of.unknown    <- function(x, y) x
#' @method vec_type2.list_of list_of
#' @export
vec_type2.list_of.list_of <- function(x, y) {
  type <- vec_type2(attr(x, "ptype"), attr(y, "ptype"))
  new_list_of(list(), type)
}
#' @method vec_type2.list_of default
#' @export
vec_type2.list_of.default  <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @rdname list_of
#' @export vec_cast.list_of
#' @method vec_cast list_of
#' @export
vec_cast.list_of <- function(x, to) {
  UseMethod("vec_cast.list_of")
}
#' @export
#' @method vec_cast.list_of NULL
vec_cast.list_of.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.list_of list
vec_cast.list_of.list <- function(x, to) {
  as_list_of(x, .ptype = attr(to, "ptype"))
}
#' @export
#' @method vec_cast.list_of list_of
vec_cast.list_of.list_of <- vec_cast.list_of.list
#' @export
#' @method vec_cast.list default
vec_cast.list_of.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}
