#' Parallel `any()` and `all()`
#'
#' @description
#' These functions are variants of [any()] and [all()] that work in parallel on
#' multiple inputs at once. They work similarly to how [pmin()] and [pmax()] are
#' parallel variants of [min()] and [max()].
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A list of logical vectors of equal size.
#'
#' @param missing Handling of missing values. One of:
#'
#'   - `NULL`, no special behavior is applied. Missings are treated the same way
#'     as `|` or `&`.
#'
#'   - `FALSE` to treat missing values as `FALSE`.
#'
#'   - `TRUE` to treat missing values as `TRUE`.
#'
#' @param size An optional output size. Only useful to specify if `x` could be
#'   an empty list.
#'
#' @param x_arg Argument name used in error messages.
#'
#' @returns A logical vector the same size as the vectors in `x`.
#'
#' @details
#' `list_pany()` and `list_pall()` are consistent with [any()] and [all()] when
#' there are no inputs to process in parallel:
#'
#' - `any()` returns `FALSE` with no inputs. Similarly,
#'   `list_pany(list(), size = 1)` returns `FALSE`.
#'
#' - `all()` returns `TRUE` with no inputs. Similarly,
#'   `list_pall(list(), size = 1)` returns `TRUE`.
#'
#' @name parallel-operators
#'
#' @examples
#' a <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
#' b <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)
#' x <- list(a, b)
#'
#' # Default behavior treats missings like `|` does
#' list_pany(x)
#' a | b
#'
#' # Default behavior treats missings like `&` does
#' list_pall(x)
#' a & b
#'
#' # Remove missings from the computation, like `na_rm = TRUE`
#' list_pany(x, missing = FALSE)
#' (a & !is.na(a)) | (b & !is.na(b))
#'
#' list_pall(x, missing = TRUE)
#' (a | is.na(a)) & (b | is.na(b))
#'
#' # `list_pall()` can be used to implement a `dplyr::filter()` style API
#' df <- data_frame(id = seq_along(a), a = a, b = b)
#'
#' keep_rows <- function(x, ...) {
#'   vec_slice(x, list_pall(list(...), missing = FALSE))
#' }
#' drop_rows <- function(x, ...) {
#'   vec_slice(x, !list_pall(list(...), missing = FALSE))
#' }
#'
#' # "Keep / Drop the rows when both a and b are TRUE"
#' # These form complements of one another, even with `NA`s.
#' keep_rows(df, a, b)
#' drop_rows(df, a, b)
#'
#' # Same empty behavior as `any()` and `all()`
#' list_pany(list(), size = 1)
#' any()
#'
#' list_pall(list(), size = 1)
#' all()
NULL

#' @rdname parallel-operators
#' @export
list_pany <- function(
  x,
  ...,
  missing = NULL,
  size = NULL,
  x_arg = caller_arg(x),
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(ffi_list_pany, x, missing, size, environment())
}

#' @rdname parallel-operators
#' @export
list_pall <- function(
  x,
  ...,
  missing = NULL,
  size = NULL,
  x_arg = caller_arg(x),
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(ffi_list_pall, x, missing, size, environment())
}
