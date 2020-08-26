#' Table S3 class
#'
#' These functions help the base table class fit into the vctrs type system
#' by providing coercion and casting functions.
#'
#' @keywords internal
#' @name table
NULL

#' @export
vec_restore.table <- function(x, to, ...) {
  new_table(x, dim = dim(x), dimnames = dimnames(x))
}

# Print -------------------------------------------------------------------

#' @export
vec_ptype_full.table <- function(x, ...) {
  paste0("table", vec_ptype_shape(x))
}

#' @export
vec_ptype_abbr.table <- function(x, ...) {
  "table"
}

# Coercion ----------------------------------------------------------------

#' @export
vec_ptype2.table.table <- function(x, y, ..., x_arg = "", y_arg = "") {
  ptype <- vec_ptype2(unstructure(x), unstructure(y))
  vec_shaped_ptype(new_table(ptype), x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @export
vec_cast.table.table <- function(x, to, ...) {
  out <- vec_cast(unstructure(x), unstructure(to))
  out <- new_table(out, dim = dim(x), dimnames = dimnames(x))
  shape_broadcast(out, to, ...)
}

# ------------------------------------------------------------------------------

new_table <- function(x = integer(), dim = NULL, dimnames = NULL) {
  if (is_null(dim)) {
    dim <- length(x)
  } else if (!is.integer(dim)) {
    abort("`dim` must be an integer vector.")
  }

  dimnames <- dimnames %||% vec_init(list(), length(dim))

  n_elements <- prod(dim)
  n_x <- length(x)

  if (n_elements != n_x) {
    abort(glue::glue(
      "Length implied by `dim`, {n_elements}, must match the length of `x`, {n_x}."
    ))
  }

  structure(x, dim = dim, dimnames = dimnames, class = "table")
}

is_bare_table <- function(x) {
  identical(class(x), "table")
}
