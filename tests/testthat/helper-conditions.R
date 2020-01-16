
with_subscript_arg <- function(expr, arg) {
  local_options(rlang_force_unhandled_error = TRUE)
  tryCatch(
    expr,
    vctrs_error_subscript = function(cnd) {
      cnd$arg <- arg
      cnd_signal(cnd)
    }
  )
}
