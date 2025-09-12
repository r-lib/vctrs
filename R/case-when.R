#' Recode and replace using logical conditions
#'
#' @description
#'
#' - `vec_case_when()` constructs an entirely new vector by recoding the `TRUE`
#'   `cases` to their corresponding `values`. If there are locations not matched
#'   by `cases`, then they are recoded to the `default` value.
#'
#' - `vec_replace_when()` updates an existing vector by replacing the values
#'   from `x` matched by the `TRUE` `cases` with their corresponding `values`.
#'   In this case, each element of `values` must have the same type as `x` and
#'   locations not matched by `cases` retain their original `x` value.
#'
#' `vec_case_when()` is often thought of as a way to vectorize multiple if-else
#' statements, and is an R equivalent of the SQL "searched" `CASE WHEN`
#' statement.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A vector.
#'
#' @param cases A list of logical vectors.
#'
#'   For `vec_case_when()`, each vector should be the same size.
#'
#'   For `vec_replace_when()`, each vector should be the same size as `x`.
#'
#'   Where a value in `cases` is `TRUE`, the corresponding value in `values`
#'   will be assigned to the result.
#'
#' @param values A list of vectors.
#'
#'   For `vec_case_when()`, each vector should be size 1 or the size implied by
#'   `cases`. The common type of `values` and `default` determine the output
#'   type, unless overridden by `ptype`.
#'
#'   For `vec_replace_when()`, each vector should be size 1 or the same size
#'   as `x`. Each vector will be cast to the type of `x`.
#'
#' @param default Default value to use when `cases` does not match every
#'   location in the output.
#'
#'   By default, a missing value is used as the default value.
#'
#'   If supplied, `default` must be size 1 or the size implied by `cases`.
#'
#'   Can only be set when `unmatched = "default"`.
#'
#' @param unmatched Handling of unmatched locations.
#'
#'   One of:
#'
#'   - `"default"` to use `default` in unmatched locations.
#'
#'   - `"error"` to error when there are unmatched locations.
#'
#' @param ptype An optional override for the output type, which is usually
#'   computed as the common type of `values` and `default`.
#'
#' @param size An optional override for the output size, which is usually
#'   computed as the size of the first element of `cases`.
#'
#'   Only useful for requiring a fixed size when `cases` is an empty list.
#'
#' @param x_arg,cases_arg,values_arg,default_arg Argument names used in error
#'   messages.
#'
#' @returns
#' A vector.
#'
#' - For `vec_case_when()`, the type of the output is computed as the common
#'   type of `values` and `default`, unless overridden by `ptype`. The names of
#'   the output come from the names of `values` and `default`. The size of the
#'   output comes from the implied size from `cases`, unless overridden by
#'   `size`.
#'
#' - For `vec_replace_when()`, the type of the output will have the same type as
#'   `x`. The names of the output will be the same as the names of `x`. The size
#'   of the output will be the same size as `x`.
#'
#' @name vec-case-and-replace
#'
#' @examples
#' # Note how the first `TRUE` is used in the output.
#' # Also note how the `NA` falls through to `default`.
#' x <- seq(-2L, 2L, by = 1L)
#' x <- c(x, NA)
#' cases <- list(
#'   x < 0,
#'   x < 1
#' )
#' values <- list(
#'   "<0",
#'   "<1"
#' )
#' vec_case_when(
#'   cases,
#'   values,
#'   default = "other"
#' )
#'
#' # Missing values need to be handled with their own case
#' # if you want them to have a special value
#' cases <- list(
#'   x < 0,
#'   x < 1,
#'   is.na(x)
#' )
#' values <- list(
#'   "<0",
#'   "<1",
#'   NA
#' )
#' vec_case_when(
#'   cases,
#'   values,
#'   default = "other"
#' )
#'
#' # Both `values` and `default` are vectorized
#' values <- list(
#'   x * 5,
#'   x * 10,
#'   NA
#' )
#' vec_case_when(
#'   cases,
#'   values,
#'   default = x * 100
#' )
#'
#' # Use `vec_replace_when()` if you need to update `x`, retaining
#' # all previous values in locations that you don't match
#' cases <- list(
#'   x < 0,
#'   x < 1
#' )
#' values <- list(
#'   0,
#'   1
#' )
#' out <- vec_replace_when(
#'   x,
#'   cases,
#'   values
#' )
#' out
#'
#' # Note how `vec_replace_when()` is type stable on `x`, we retain the
#' # integer type here even though `values` contained doubles
#' typeof(out)
#'
#' # `vec_case_when()` creates a new vector, so names come from `values`
#' # and `default`. `vec_replace_when()` modifies an existing vector, so
#' # names come from `x` no matter what, just like `[<-` and `base::replace()`
#' x <- c(a = 1, b = 2, c = 3)
#' cases <- list(x == 1, x == 2)
#' values <- list(c(x = 0), c(y = -1))
#' vec_case_when(cases, values)
#' vec_replace_when(x, cases, values)
#'
#' # If you want to enforce that you've covered all of the locations in your
#' # `cases`, use `unmatched = "error"` rather than providing a `default`
#' x <- c(0, 1, 2)
#' cases <- list(x == 1, x == 2)
#' values <- list("a", "b")
#' try(vec_case_when(cases, values, unmatched = "error"))
NULL

#' @rdname vec-case-and-replace
#' @export
vec_case_when <- function(
  cases,
  values,
  ...,
  default = NULL,
  unmatched = "default",
  ptype = NULL,
  size = NULL,
  cases_arg = "cases",
  values_arg = "values",
  default_arg = "default",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_vec_case_when,
    cases,
    values,
    default,
    unmatched,
    ptype,
    size,
    environment()
  )
}

#' @rdname vec-case-and-replace
#' @export
vec_replace_when <- function(
  x,
  cases,
  values,
  ...,
  x_arg = "x",
  cases_arg = "cases",
  values_arg = "values",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_vec_replace_when,
    x,
    cases,
    values,
    environment()
  )
}
