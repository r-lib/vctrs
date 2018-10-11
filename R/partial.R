#' Partial type
#'
#' Use `new_partial()` when constructing a new partial type subclass;
#' and use `is_partial()` to test if an type is partial. All subclasses
#' need to provide a `vec_type_finalise()` method.
#'
#' As the name suggests, a partial type _partially_ specifies a type, and
#' it must be combined with data to yield a full type. A useful example
#' of a partial type is [partial_frame()], which makes it possible to
#' specify the type of just a few columns in a data frame. Use this constructor
#' if you're making your own partial type.
#'
#' @param ... Attributes of the partial type
#' @param class Name of subclass.
#' @export
#' @keywords internal
new_partial <- function(..., class = character()) {
  new_sclr(..., class = c(class, "vctrs_partial"))
}

#' @export
obj_print_header.vctrs_partial <- function(x, ...) {
  NULL
  invisible(x)
}

#' @export
obj_print_data.vctrs_partial <- function(x, ...) {
  cat_line(vec_ptype_full(x))
  invisible(x)
}

#' @rdname new_partial
#' @export
is_partial <- function(x) {
  is.null(x) || inherits(x, "vctrs_partial")
}

#' @rdname new_partial
#' @export
vec_type_finalise <- function(x) {
  UseMethod("vec_type_finalise")
}

#' @export
vec_type_finalise.default <- function(x) {
  x
}

#' @export
vec_type_finalise.vctrs_partial <- function(x) {
  stop_unimplemented(x, "vec_type_finalise")
}
