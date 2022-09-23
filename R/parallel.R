#' Parallel `any()` and `all()`
#'
#' @description
#' These functions are variants of [any()] and [all()] that work in parallel on
#' multiple inputs at once. They work similarly to how [pmin()] and [pmax()] are
#' parallel variants of [min()] and [max()].
#'
#' @details
#' `vec_pany()` and `vec_pall()` are consistent with [any()] and [all()] when
#' there are no inputs to process in parallel:
#'
#' - `any()` returns `FALSE` with no inputs. Similarly, `vec_pany(.size = 1)`
#'   and `vec_pany(NA, .na_rm = TRUE)` both return `FALSE`.
#'
#' - `all()` returns `TRUE` with no inputs. Similarly, `vec_pall(.size = 1)`
#'   and `vec_pall(NA, .na_rm = TRUE)` both return `TRUE`.
#'
#' @param ... Logical vectors. These will be [recycled][vector_recycling_rules]
#'   to their common size.
#'
#' @param .na_rm Should missing values be removed?
#'
#' @param .size An optional output size that overrides the common size of the
#'   inputs in `...`.
#'
#' @name parallel-operators
#'
#' @examples
#' x <- c(TRUE, FALSE, NA, TRUE, NA)
#' y <- c(FALSE, FALSE, TRUE, TRUE, NA)
#'
#' vec_pany(x, y)
#' vec_pall(x, y)
#'
#' # Missing values can be removed from the computation
#' vec_pany(x, y, .na_rm = TRUE)
#' vec_pall(x, y, .na_rm = TRUE)
#'
#' # Same empty behavior as `any()` and `all()`
#' vec_pany(.size = 1)
#' any()
#'
#' vec_pall(.size = 1)
#' all()
NULL

#' @rdname parallel-operators
#' @export
vec_pany <- function(..., .na_rm = FALSE, .size = NULL) {
  .Call(ffi_vec_pany, list2(...), .na_rm, .size, environment())
}

#' @rdname parallel-operators
#' @export
vec_pall <- function(..., .na_rm = FALSE, .size = NULL) {
  .Call(ffi_vec_pall, list2(...), .na_rm, .size, environment())
}
