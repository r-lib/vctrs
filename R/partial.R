#' Partial type
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Use `new_partial()` when constructing a new partial type subclass;
#' and use `is_partial()` to test if a type is partial. All subclasses
#' need to provide a `vec_ptype_finalise()` method.
#'
#' @details
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
  .Call(ffi_is_partial, x)
}

#' @rdname new_partial
#' @inheritParams rlang::args_dots_empty
#' @export
vec_ptype_finalise <- function(x, ...) {
  check_dots_empty0(...)
  return(.Call(vctrs_ptype_finalise, x))
  UseMethod("vec_ptype_finalise")
}
vec_ptype_finalise_dispatch <- function(x, ...) {
  UseMethod("vec_ptype_finalise")
}
#' @export
vec_ptype_finalise.vctrs_partial <- function(x, ...) {
  # nocov start
  stop_unimplemented(x, "vec_ptype_finalise")
  # nocov end
}

#' @export
vec_ptype_finalise.default <- function(x, ...) {
  x
}
