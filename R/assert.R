#' Assert an argument has known prototype and/or size
#'
#' @description
#'
#' * `vec_is()` is a predicate that checks if its input conforms to a
#'   prototype and/or a size.
#'
#' * `vec_assert()` throws an error when the input doesn't conform.
#'
#' @section Error types:
#'
#' * If the prototype doesn't match, an error of class
#'   `"vctrs_error_assert_ptype"` is raised.
#'
#' * If the prototype doesn't match, an error of class
#' `"vctrs_error_assert_size"` is raised.
#'
#' Both errors inherit from `"vctrs_error_assert"`.
#'
#' @param x A vector argument to check.
#' @param ptype Prototype to compare against.
#' @param size Size to compare against
#' @param arg Name of argument being checked. This is used in error
#'   messages. The label of the expression passed as `x` is taken as
#'   default.
#'
#' @return `vec_is()` returns `TRUE` or `FALSE`. `vec_assert()` either
#'   throws a typed error (see section on error types) or returns `x`,
#'   invisibly.
#' @export
vec_assert <- function(x, ptype = NULL, size = NULL, arg = NULL) {
  arg <- arg %||% as_label(substitute(x))

  if (!has_ptype(x, ptype)) {
    ptype <- vec_type(ptype)
    x_type <- vec_type(x)
    msg <- paste0("`", arg, "` must be <", vec_ptype_abbr(ptype), ">, not <", vec_ptype_abbr(x_type), ">.")
    abort(
      msg,
      .subclass = c("vctrs_error_assert_ptype", "vctrs_error_assert"),
      required = ptype,
      actual = x_type
    )
  }

  if (!has_size(x, size)) {
    size <- vec_recycle(vec_cast(size, integer()), 1L)
    x_size <- vec_size(x)
    msg <- paste0("`", arg, "` must have size ", size, ", not size ", x_size, ".")
    abort(
      msg,
      .subclass = c("vctrs_error_assert_size", "vctrs_error_assert"),
      required = size,
      actual = x_size
    )
  }

  invisible(x)
}
#' @export
vec_is <- function(x, ptype = NULL, size = NULL) {
  has_ptype(x, ptype) && has_size(x, size)
}

has_ptype <- function(x, ptype = NULL) {
  ptype <- vec_type(ptype)
  x_type <- vec_type(x)
  is_null(ptype) || identical(ptype, x_type)
}
has_size <- function(x, size = NULL) {
  size <- vec_recycle(vec_cast(size, integer()), 1L)
  x_size <- vec_size(x)
  is_null(size) || identical(size, x_size)
}
