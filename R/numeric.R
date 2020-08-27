#' Mathematical operations
#'
#' This generic provides a common dispatch mechanism for all regular unary
#' mathematical functions. It is used as a common wrapper around many of the
#' Summary group generics, the Math group generics, and a handful of other
#' mathematical functions like `mean()` (but not `var()` or `sd()`).
#'
#' `vec_math_base()` is provided as a convenience for writing methods. It
#' calls the base `.fn` on the underlying [vec_data()].
#'
#' @section Included functions:
#'
#' * From the [Summary] group generic:
#'   `prod()`, `sum()`, `any()`, `all()`.
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
#' Note that `median()` is currently not implemented, and `sd()` and
#' `var()` are currently not generic and so do not support custom
#' classes.
#'
#' @seealso [vec_arith()] for the equivalent for the arithmetic infix operators.
#' @param .fn A mathematical function from the base package, as a string.
#' @param .x A vector.
#' @param ... Additional arguments passed to `.fn`.
#' @keywords internal
#' @export
#' @examples
#' x <- new_vctr(c(1, 2.5, 10))
#' x
#'
#' abs(x)
#' sum(x)
#' cumsum(x)
vec_math <- function(.fn, .x, ...) {
  UseMethod("vec_math", .x)
}

#' @export
vec_math.default <- function(.fn, .x, ...) {
  if (!is_double(.x) && !is_logical_dispatch(.fn, .x)) {
    stop_unimplemented(.x, "vec_math")
  }

  out <- vec_math_base(.fn, .x, ...)

  # Don't restore output of logical predicates like `any()`,
  # `is.finite()`, or `is.nan()`
  if (is_double(out)) {
    out <- vec_restore(out, .x)
  }

  out
}
is_logical_dispatch <- function(fn, x) {
  is_logical(x) && fn %in% c("any", "all")
}

#' @export
#' @rdname vec_math
vec_math_base <- function(.fn, .x, ...) {
  .fn <- getExportedValue("base", .fn)
  .fn(vec_data(.x), ...)
}
