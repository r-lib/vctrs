#' Vectorized if-else
#'
#' @description
#' `vec_if_else()` is a vectorized [if-else][if]. Compared to the base R
#' equivalent, [ifelse()], this function allows you to handle missing values in
#' the `condition` with `missing` and always takes `true`, `false`, and
#' `missing` into account when determining what the output type should be.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param condition A logical vector.
#'
#' @param true,false Vectors to use for `TRUE` and `FALSE` values of
#'   `condition`.
#'
#'   Both `true` and `false` will be [recycled][theory-faq-recycling]
#'   to the size of `condition`.
#'
#'   `true`, `false`, and `missing` (if used) will be cast to their common type.
#'
#' @param missing If not `NULL`, will be used as the value for `NA` values of
#'   `condition`. Follows the same size and type rules as `true` and `false`.
#'
#' @param ptype An optional prototype declaring the desired output type. If
#'   supplied, this overrides the common type of `true`, `false`, and `missing`.
#'
#' @param condition_arg,true_arg,false_arg,missing_arg Argument names used in
#'   error messages.
#'
#' @returns
#' A vector with the same size as `condition` and the same type as the common
#' type of `true`, `false`, and `missing`.
#'
#' Where `condition` is `TRUE`, the matching values from `true`, where it is
#' `FALSE`, the matching values from `false`, and where it is `NA`, the matching
#' values from `missing`, if provided, otherwise a missing value will be used.
#'
#' @export
#' @examples
#' x <- c(-5:5, NA)
#' vec_if_else(x < 0, NA, x)
#'
#' # Explicitly handle `NA` values in the `condition` with `missing`
#' vec_if_else(x < 0, "negative", "positive", missing = "missing")
#'
#' # Unlike `ifelse()`, `vec_if_else()` preserves types
#' x <- factor(sample(letters[1:5], 10, replace = TRUE))
#' ifelse(x %in% c("a", "b", "c"), x, NA)
#' vec_if_else(x %in% c("a", "b", "c"), x, NA)
#'
#' # `vec_if_else()` also works with data frames
#' condition <- c(TRUE, FALSE, NA, TRUE)
#' true <- data_frame(x = 1:4, y = 5:8)
#' false <- data_frame(x = 9:12, y = 13:16)
#' vec_if_else(condition, true, false)
vec_if_else <- function(
  condition,
  true,
  false,
  ...,
  missing = NULL,
  ptype = NULL,
  condition_arg = "condition",
  true_arg = "true",
  false_arg = "false",
  missing_arg = "missing",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_vec_if_else,
    condition,
    true,
    false,
    missing,
    ptype,
    environment()
  )
}
