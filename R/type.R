# The type of a vector is a compact string representation
# Compared to type_sum it is not designed to fit in a column label
# So can be quite a lot longer

vec_type <- function(x) {
  stopifnot(is_vector(x) || is_null(x))

  new_vec_type(vec_subset(x, 0L))
}

new_vec_type <- function(prototype) {
  structure(
    list(prototype = prototype),
    class = "vec_type"
  )
}

as_vec_type <- function(x) UseMethod("as_vec_type")
as_vec_type.vec_type <- function(x) x
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
max.vec_type <- function(..., na.rm = FALSE) {
  args <- list2(...)
  reduce(args, vectype_max, .init = vec_type(NULL))
}
