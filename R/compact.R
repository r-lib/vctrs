#' Drop missing or empty elements
#'
#' `list_compact()` removes missing or empty elements from a list.
#'
#' @inheritParams ellipsis::dots_empty
#'
#' @param x A list.
#'
#' @param drop A string specifying the elements to drop. One of:
#'
#'   - `"missing"`: Drop all `NULL` elements.
#'
#'   - `"empty"`: Drop all size 0 elements. This includes `NULL` elements along
#'     with empty vectors like `integer(0)`.
#'
#' @export
#' @examples
#' x <- list(1, NULL, integer(), 2)
#'
#' # By default, only `NULL` elements are dropped
#' list_compact(x)
#'
#' # You can also drop any empty element
#' list_compact(x, drop = "empty")
list_compact <- function(x, ..., drop = c("missing", "empty")) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  drop <- arg_match0(drop, values = c("missing", "empty"), arg_nm = "drop")

  .Call(vctrs_list_compact, x, drop)
}
