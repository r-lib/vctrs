#' Tools for accessing the fields of a record.
#'
#' A [rcrd] behaves like a vector, so `length()`, `names()`, and `$` can
#' not provide access to the fields of the underlying list. These helpers do:
#' `fields()` is equivalent to `names()`; `n_fields()` is equivalent to
#' `length()`; `field()` is equivalent to `$`.
#'
#' @param x A [rcrd], i.e. a list of equal length vectors with unique names.
#' @keywords internal
#' @export
#' @examples
#' x <- new_rcrd(list(x = 1:3, y = 3:1, z = letters[1:3]))
#' n_fields(x)
#' fields(x)
#'
#' field(x, "y")
#' field(x, "y") <- runif(3)
#' field(x, "y")
fields <- function(x) {
  .Call(vctrs_fields, x)
}

#' @export
#' @rdname fields
n_fields <- function(x) {
  .Call(vctrs_n_fields, x)
}

#' @export
#' @rdname fields
field <- function(x, i) {
  .Call(vctrs_field_get, x, i)
}

#' @export
#' @rdname fields
`field<-` <- function(x, i, value) {
  .Call(vctrs_field_set, x, i, value)
}
