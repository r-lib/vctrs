#' Compute "type" of a vector
#'
#' The type of a vector can be represented using a 0-row subset.
#' `new_vec_type()` is the low-level S3 constructor, `vec_type()` is a helper
#' for interactive exploration, and `as_vec_type()` for programming.
#' Use `max()` to compute the "maximum" type of a set of types; this will
#' return an error if no maximum type exists.
#'
#' @export
#' @param x For `vec_type()` a vector; for `as_vec_type()` a vector or
#'    vectory type
#' @param strict By default `max()` will error if no common type is found;
#'   `strict = FALSE` activates a more flexible mode which will always return
#'   a common type, falling back to a list if nothing more specific is
#'   available.
#' @keywords internal
#' @examples
#' vec_type(mtcars)
#'
#' max(vec_type(1), vec_type(1L))
#' \dontrun{
#' max(vec_type(1), vec_type("a"))
#' }
#' max(vec_type(1), vec_type("a"), strict = FALSE)
#'
vec_type <- function(x) {
  stopifnot(is_vector(x) || is_null(x))

  new_vec_type(vec_subset(x, 0L))
}

#' @export
#' @rdname vec_type
new_vec_type <- function(prototype) {
  structure(
    list(prototype = prototype),
    class = "vec_type"
  )
}

#' @export
#' @rdname vec_type
as_vec_type <- function(x) UseMethod("as_vec_type")

#' @export
#' @rdname as_vec_type
as_vec_type.vec_type <- function(x) x

#' @export
#' @rdname as_vec_type
as_vec_type.default <- function(x) vec_type(x)

#' @export
format.vec_type <- function(x, ...) {
  vec_type_string(x$prototype)
}

#' @export
print.vec_type <- function(x, ...) {
  cat("type: ", format(x), "\n", sep = "")
  invisible(x)
}

#' @export
#' @rdname vec_type
max.vec_type <- function(..., na.rm = FALSE, strict = TRUE) {
  args <- list2(...)
  reduce(args, vectype_max, .init = vec_type(NULL), strict = strict)
}
