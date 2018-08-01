#' Construct "repeated" objects
#'
#' A repeated object is a list where each element has the same type.
#' Modifying the list with `$`, `[`, and `[[` preserves the constraint
#' by coercing all input items.
#'
#' @inheritParams vec_c
#' @param x For `as_repeated()`, a vector to be coerced to repeated.
#' @export
#' @examples
#' x <- repeated(1:3, 5:6, 10:15)
#' tibble::tibble(x = x)
#'
#' vec_c(repeated(1, 2), repeated(FALSE, TRUE))
repeated <- function(..., .type = NULL) {
  args <- list2(...)

  type <- find_type(args, .type = .type)
  if (is_null(type)) {
    stop("Could not find common type for elements of `x`", call. = FALSE)
  }

  x <- map(args, vec_cast, to = type)
  new_repeated(x, type)
}

#' @export
#' @rdname repeated
as_repeated <- function(x, ...) {
  UseMethod("as_repeated")
}

#' @export
as_repeated.repeated <- function(x, .type = NULL, ...) {
  if (!is.null(.type)) {
    repeated(!!!x, .type = .type)
  } else {
    x
  }
}

#' @export
as_repeated.list <- function(x, ..., .type = NULL) {
  repeated(!!!x, .type = .type)
}

#' @export
#' @rdname repeated
new_repeated <- function(x, .type) {
  stopifnot(is.list(x))
  stopifnot(vec_length(.type) == 0)

  structure(
    x,
    type = .type,
    class = "repeated"
  )
}

#' @export
#' @rdname repeated
is_repeated <- function(x) {
  inherits(x, "repeated")
}

# registered .onLoad
type_sum.repeated <- function(x) {
  paste0("list<", tibble::type_sum(attr(x, "type")), ">")
}

#' @export
vec_type_string.repeated <- function(x) {
  paste0("repeated<", vec_type(attr(x, "type")), ">")
}

#' @export
print.repeated <- function(x, ...) {
  cat(format(vec_type(x)), "\n", sep = "")

  # Expensive: need to find a better way
  attr(x, "type") <- NULL
  class(x) <- NULL

  print(x)
}

#' @export
as.list.repeated <- function(x, ...) {
  attr(x, "type") <- NULL
  attr(x, "class") <- NULL
  x
}

#' @export
`[.repeated` <- function(x, ...) {
  new_repeated(NextMethod(), attr(x, "type"))
}

#' @export
`[<-.repeated` <- function(x, i, value) {
  value <- map(value, vec_cast, attr(x, "type"))
  NextMethod()
}

#' @export
`[[<-.repeated` <- function(x, i, value) {
  value <- vec_cast(value, attr(x, "type"))
  NextMethod()
}

#' @export
`$<-.repeated` <- function(x, i, value) {
  value <- vec_cast(value, attr(x, "type"))
  NextMethod()
}
