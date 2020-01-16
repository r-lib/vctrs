
with_subscript_data <- function(expr,
                                arg,
                                subscript_elt = NULL,
                                subscript_input = NULL) {
  local_options(rlang_force_unhandled_error = TRUE)
  tryCatch(
    expr,
    vctrs_error_subscript = function(cnd) {
      cnd$arg <- arg
      cnd$subscript_elt <- subscript_elt
      cnd_signal(cnd)
    }
  )
}

with_tibble_cols <- function(expr) {
  with_subscript_data(
    expr,
    arg = quote(tbl[i]),
    subscript_elt = c("column", "columns"),
    subscript_input = "tibble"
  )
}
with_tibble_rows <- function(expr) {
  with_subscript_data(
    expr,
    arg = quote(tbl[i]),
    subscript_elt = c("row", "rows"),
    subscript_input = "tibble"
  )
}
