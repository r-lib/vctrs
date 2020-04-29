#' Table S3 class
#'
#' These functions help the base table class fit into the vctrs type system
#' by providing coercion and casting functions.
#'
#' @keywords internal
#' @name table
NULL

# ------------------------------------------------------------------------------
# Print

#' @export
vec_ptype_full.table <- function(x, ...) {
  paste0("table", vec_ptype_shape(x))
}

#' @export
vec_ptype_abbr.table <- function(x, ...) {
  "table"
}

# ------------------------------------------------------------------------------
# Coercion

#' @rdname table
#' @export vec_ptype2.table
#' @method vec_ptype2 table
#' @export
vec_ptype2.table <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.table")
}

#' @method vec_ptype2.table table
#' @export
vec_ptype2.table.table <- function(x, y, ..., x_arg = "", y_arg = "") {
  vec_shaped_ptype(new_table(), x, y, x_arg = x_arg, y_arg = y_arg)
}

# ------------------------------------------------------------------------------
# Cast

#' @rdname table
#' @export vec_cast.table
#' @method vec_cast table
#' @export
vec_cast.table <- function(x, to, ..., x_arg = "", to_arg = "") {
  UseMethod("vec_cast.table")
}
#' @method vec_cast.table table
#' @export
vec_cast.table.table <- function(x, to, ...) {
  shape_broadcast(x, to, ...)
}

# ------------------------------------------------------------------------------

new_table <- function(x = integer(), dim = 0L) {
  # `table()` doesn't support long vectors, even though `tabulate()` does
  if (!is_integer(x)) {
    abort("`x` must be an integer vector.")
  }

  if (!is.integer(dim)) {
    abort("`dim` must be an integer vector.")
  }

  n_elements <- prod(dim)
  n_x <- length(x)

  if (n_elements != n_x) {
    abort(glue::glue(
      "Length implied by `dim`, {n_elements}, must match the length of `x`, {n_x}."
    ))
  }

  structure(x, dim = dim, class = "table")
}

is_bare_table <- function(x) {
  identical(class(x), "table")
}
