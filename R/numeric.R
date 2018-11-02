#' Mathematical operations
#'
#' This generic provides a common dispatch mechanism for all regular unary
#' mathematical functions. It is used as a common wrapper around the Summary
#' group generics, the Math group generics, and a handful of other
#' mathematical functions like `mean()`.
#'
#' `vec_base_arith()` is provided as a convenience for writing methods. It
#' calls the base `fun` on the underlying [vec_data()].
#'
#' @section Included functions:
#'
#' * From the [Summary] group generic:
#'   `max()`, `min()`, `range()`, `prod`, `sum()`, `any()`, `all()`.
#'
#' * From the [Math] group generic:
#'   `abs()`, `sign()`, `sqrt()`, `ceiling()`, `floor()`, `trunc()`, `cummax()`,
#'   `cummin()`, `cumprod()`, `cumsum()`, `log()`, `log10()`, `log2()`,
#'   `log1p()`, `acos()`, `acosh()`, `asin()`, `asinh()`, `atan()`, `atanh()`,
#'   `exp()`, `expm1()`, `cos()`, `cosh()`, `cospi()`, `sin()`, `sinh()`,
#'   `sinpi()`, `tan()`, `tanh()`, `tanpi()`, `gamma()`, `lgamma()`,
#'   `digamma()`, `trigamma()`.
#'
#' * Additional generics: `mean()`, `is.nan()`, `is.finite()`, `is.infinite()`.
#'
#' @seealso [vec_arith()] for the equivalent for the arithmetic infix operators.
#' @param fun An mathematical function as a string
#' @param x A vector
#' @param ... An additional arguments.
#' @keywords internal
#' @export
#' @examples
#' x <- new_vctr(c(1, 2.5, 10))
#' x
#'
#' abs(x)
#' sum(x)
#' cumsum(x)
vec_math <- function(fun, x, ...) {
  UseMethod("vec_math", x)
}

#' @export
vec_math.default <- function(fun, x, ...) {
  if (is_double(x)) {
    vec_restore(vec_math_base(fun, x, ...), x)
  } else {
    stop_unimplemented(x, "vec_math")
  }
}

#' @export
#' @rdname vec_math
vec_math_base <- function(fun, x, ...) {
  fun <- getExportedValue("base", fun)
  fun(vec_data(x), ...)
}
