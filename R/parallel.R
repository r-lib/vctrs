#' Parallel `any()` and `all()`
#'
#' @description
#' These functions are variants of [any()] and [all()] that work in parallel on
#' multiple inputs at once. They work similarly to how [pmin()] and [pmax()] are
#' parallel variants of [min()] and [max()].
#'
#' @inheritParams rlang::args_error_context
#'
#' @param ... Logical vectors of equal size.
#'
#' @param .missing Value to use when a missing value is encountered. One of:
#'
#'   - `NA` to propagate missing values. With this, missings are treated the
#'     same way as `|` or `&`.
#'
#'   - `FALSE` to treat missing values as `FALSE`.
#'
#'   - `TRUE` to treat missing values as `TRUE`.
#'
#' @param .size An optional output size. Only useful to specify if it is possible
#'   for no inputs to be provided.
#'
#' @param .arg Argument name used in error messages.
#'
#' @returns A logical vector the same size as the vectors in `...`.
#'
#' @details
#' `vec_pany()` and `vec_pall()` are consistent with [any()] and [all()] when
#' there are no inputs to process in parallel:
#'
#' - `any()` returns `FALSE` with no inputs. Similarly, `vec_pany(.size = 1)`
#'   returns `FALSE`.
#'
#' - `all()` returns `TRUE` with no inputs. Similarly, `vec_pall(.size = 1)`
#'   returns `TRUE`.
#'
#' @name parallel-operators
#'
#' @examples
#' a <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
#' b <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)
#'
#' # Default behavior treats missings like `|` does
#' vec_pany(a, b)
#' a | b
#'
#' # Default behavior treats missings like `&` does
#' vec_pall(a, b)
#' a & b
#'
#' # Remove missings from the computation, like `na_rm = TRUE`
#' vec_pany(a, b, .missing = FALSE)
#' (a & !is.na(a)) | (b & !is.na(b))
#'
#' vec_pall(a, b, .missing = TRUE)
#' (a | is.na(a)) & (b | is.na(b))
#'
#' # `vec_pall()` can be used to implement a `dplyr::filter()` style API
#' df <- data_frame(id = seq_along(a), a = a, b = b)
#'
#' keep_rows <- function(x, ...) {
#'   vec_slice(x, vec_pall(..., .missing = FALSE))
#' }
#' drop_rows <- function(x, ...) {
#'   vec_slice(x, !vec_pall(..., .missing = FALSE))
#' }
#'
#' # "Keep / Drop the rows when both a and b are TRUE"
#' # These form complements of one another, even with `NA`s.
#' keep_rows(df, a, b)
#' drop_rows(df, a, b)
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
vec_pany <- function(
  ...,
  .missing = NA,
  .size = NULL,
  .arg = "",
  .error_call = current_env()
) {
  .Call(ffi_vec_pany, list2(...), .missing, .size, environment())
}

#' @rdname parallel-operators
#' @export
vec_pall <- function(
  ...,
  .missing = NA,
  .size = NULL,
  .arg = "",
  .error_call = current_env()
) {
  .Call(ffi_vec_pall, list2(...), .missing, .size, environment())
}
