#' vctr group generics
#'
#' These generics are inspired by the S3 [groupGeneric] functions, but use
#' `vec_ptype()` and `vec_cast()` to ensure that all inputs are of the same
#' type. They are also finer grained, additionally dividing up groups by arity
#' and return type. `vec_generic_call()` provides a convenient way to call the
#' default method of internal generics; this allows you to use existing base
#' R behaviour where possible.
#'
#' vctrs provides five group generics:
#'
#' * `vec_grp_compare()`: `==`, `!=`, `<`, `<=`, `>=`, `>`
#' * `vec_grp_logical()`: `&`, `|`, (unary) `!`
#' * `vec_grp_unary()`: unary `+` and `-`.
#' * `vec_grp_numeric()`: `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`
#' * `vec_grp_summary()`: [all()], [any()], [sum()], [prod()], [min()],
#'   [max()], [range()]`
#'
#' If your vector is number-y you will generally implement `vec_grp_unary()`,
#' `vec_grp_numeric()`, and `vec_grp_summary()`. If your vector is truth-y,
#' you will implement `vec_grp_logical()`
#'
#' @section Default methods:
#'
#' `vec_grp_compare()` and `vec_grp_logical()` call the internal method
#' and return a logical vector. `vec_grp_unary()`, `vec_grp_numeric()`, and
#' `vec_grp_summary()` will call the internal method, and then recast the
#' output to the same ptype as `x`.
#'
#' @keywords internal
#' @param generic Name of generic function
#' @param x,y Vectors. `x` and `y` will always have the same ptype.
#' @param na.rm Remove missing values?
#' @param ... Additional arguments passed to `generic`.
#' @name vec_grp
NULL

#' @rdname vec_grp
#' @export
vec_grp_compare <- function(generic, x, y) {
  UseMethod("vec_grp_compare", x)
}

#' @rdname vec_grp
#' @export
vec_grp_logical <- function(generic, x, y) {
  UseMethod("vec_grp_logical", x)
}

#' @rdname vec_grp
#' @export
vec_grp_unary <- function(generic, x) {
  UseMethod("vec_grp_unary", x)
}

#' @rdname vec_grp
#' @export
vec_grp_numeric <- function(generic, x, y) {
  UseMethod("vec_grp_numeric", x)
}

#' @rdname vec_grp
#' @export
vec_grp_summary <- function(generic, x, na.rm = TRUE) {
  UseMethod("vec_grp_summary", x)
}

#' @rdname vec_grp
#' @export
vec_generic_call <- function(generic, x, y, ...) {
  x <- vec_data(x)
  args <- list(quote(x))

  if (!missing(y)) {
    y <- vec_data(y)
    args[[2]] <- quote(y)
  }

  generic <- as.name(generic)
  call <- expr((!!generic)(!!!args, ...))
  out <- eval_bare(call)

  # Some of the Math ops form the output template by copying x, attributes
  # all. Ideally here we'd just mutate `out`, rather than creating a copy.
  if (!is.null(attr(out, "class"))) {
    attributes(out) <- NULL
  }

  out
}

# Default methods ---------------------------------------------------------

#' @export
vec_grp_compare.vctr <- function(generic, x, y) {
  vec_generic_call(generic, x, y)
}

#' @export
vec_grp_logical.vctr <- function(generic, x, y) {
  if (!is_logical(x)) {
    stop("Non-logical input to Boolean operator", call. = FALSE)
  }

  vec_generic_call(generic, x, y)
}

#' @export
vec_grp_unary.vctr <- function(generic, x) {
  if (!is_integer(x) && !is_double(x)) {
    stop(
      glue::glue("Non-numeric input to mathematical operator ({generic}x)"),
      call. = FALSE
    )
  }

  out <- vec_generic_call(generic, x)
  vec_cast(out, x)
}

#' @export
vec_grp_numeric.vctr <- function(generic, x, y) {
  if (!is_integer(x) && !is_double(x)) {
    stop(
      glue::glue("Non-numeric input to mathematical operator (x {generic} y)"),
      call. = FALSE
    )
  }

  out <- vec_generic_call(generic, x, y)
  vec_cast(out, x)
}

#' @export
vec_grp_summary.vctr <- function(generic, x, na.rm = TRUE) {
  if (!is_integer(x) && !is_double(x)) {
    stop(
      glue::glue("Non-numeric input to mathematical function `{generic}()`"),
      call. = FALSE
    )
  }

  out <- vec_generic_call(generic, x, na.rm = TRUE)
  vec_cast(out, x)
}
