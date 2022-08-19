
with_subscript_data <- function(expr,
                                subscript_arg,
                                subscript_elt = NULL,
                                subscript_action = NULL) {
  local_options(rlang_force_unhandled_error = TRUE)
  tryCatch(
    expr,
    vctrs_error_subscript = function(cnd) {
      cnd$subscript_arg <- subscript_arg
      cnd$subscript_elt <- subscript_elt
      cnd$subscript_action <- subscript_action
      cnd_signal(cnd)
    }
  )
}

with_tibble_cols <- function(expr) {
  with_subscript_data(
    expr,
    subscript_arg = quote(foo(bar)),
    subscript_elt = "column",
    subscript_action = "rename"
  )
}
with_tibble_rows <- function(expr) {
  with_subscript_data(
    expr,
    subscript_arg = quote(foo(bar)),
    subscript_elt = "row",
    subscript_action = "remove"
  )
}
with_dm_tables <- function(expr) {
  with_subscript_data(
    expr,
    subscript_arg = quote(foo(bar)),
    subscript_elt = "table",
    subscript_action = "extract"
  )
}
with_tidyselect_select <- function(expr) {
  with_subscript_data(
    expr,
    subscript_arg = quote(foo(bar)),
    subscript_elt = "column",
    subscript_action = "select"
  )
}
with_tidyselect_relocate <- function(expr) {
  with_subscript_data(
    expr,
    subscript_arg = quote(foo(bar)),
    subscript_elt = "column",
    subscript_action = "relocate"
  )
}
