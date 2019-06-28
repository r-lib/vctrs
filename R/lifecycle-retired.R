#' Is a vector empty
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("defunct")}
#'
#' This function is defunct, please use [vec_is_empty()].
#'
#' @param x An object.
#'
#' @keywords internal
#' @export
vec_empty <- function(x) {
  stop_defunct(paste_line(
    "`vec_empty()` is defunct as of vctrs 0.2.0.",
    "Please use `vec_is_empty()` instead."
  ))
}

#' Deprecated type functions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("deprecated")}
#'
#' These functions have been renamed:
#'
#' * `vec_type()` => [vec_ptype()]
#' * `vec_type2()` => [vec_ptype2()]
#' * `vec_type_common()` => [vec_ptype_common()]
#'
#' @param x,y,...,.ptype Arguments for deprecated functions.
#'
#' @keywords internal
#' @export
vec_type <- function(x) {
  warn_deprecated(c("`vec_type()` has been renamed to `vec_ptype()`."))
  vec_ptype(x)
}
#' @rdname vec_type
#' @export
vec_type_common <- function(..., .ptype = NULL) {
  warn_deprecated(c("`vec_type_common()` has been renamed to `vec_ptype_common()`."))
  vec_ptype_common(..., .ptype = .ptype)
}
#' @rdname vec_type
#' @export
vec_type2 <- function(x, y, ...) {
  warn_deprecated(c("`vec_type2()` has been renamed to `vec_ptype2()`."))
  vec_ptype2(x, y, ...)
}
