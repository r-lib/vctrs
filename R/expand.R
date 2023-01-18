#' Create a data frame from all combinations of the inputs
#'
#' @description
#' `vec_expand_grid()` is inspired by [expand.grid()]. Compared with
#' `expand.grid()`, it:
#'
#' - Produces sorted output by default by varying the first column the slowest,
#'   rather than the fastest. Control this with `.vary`.
#'
#' - Never converts strings to factors.
#'
#' - Does not add additional attributes.
#'
#' - Drops `NULL` inputs.
#'
#' - Can expand any vector type, including data frames and [records][new_rcrd].
#'
#' @details
#' If any input is empty (i.e. size 0), then the result will have 0 rows.
#'
#' If no inputs are provided, the result is a 1 row data frame with 0 columns.
#' This is consistent with the fact that `prod()` with no inputs returns `1`.
#'
#' @inheritParams rlang::args_error_context
#' @inheritParams df_list
#'
#' @param ... Name-value pairs. The name will become the column name in the
#'   resulting data frame.
#'
#' @param .vary One of:
#'
#'   - `"slowest"` to vary the first column slowest. This produces sorted
#'     output and is generally the most useful.
#'
#'   - `"fastest"` to vary the first column fastest. This matches the behavior
#'     of [expand.grid()].
#'
#' @returns
#' A data frame with as many columns as there are inputs in `...` and as many
#' rows as the [prod()] of the sizes of the inputs.
#'
#' @export
#' @examples
#' vec_expand_grid(x = 1:2, y = 1:3)
#'
#' # Use `.vary` to match `expand.grid()`:
#' vec_expand_grid(x = 1:2, y = 1:3, .vary = "fastest")
#'
#' # Can also expand data frames
#' vec_expand_grid(
#'   x = data_frame(a = 1:2, b = 3:4),
#'   y = 1:4
#' )
vec_expand_grid <- function(...,
                            .vary = "slowest",
                            .name_repair = "check_unique",
                            .error_call = current_env()) {
  .vary <- arg_match0(
    arg = .vary,
    values = c("slowest", "fastest"),
    error_call = .error_call
  )

  .Call(ffi_vec_expand_grid, list2(...), .vary, .name_repair, environment())
}
