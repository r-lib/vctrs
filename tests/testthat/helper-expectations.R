expect_dim <- function(x, shape) {
  dim <- vec_dim(x)
  expect_equal(dim, !!shape)
}

expect_lossy <- function(expr, result, x = NULL, to = NULL) {
  expr <- enquo(expr)
  expect_error(eval_tidy(expr), class = "vctrs_error_cast_lossy")

  out <- allow_lossy_cast(eval_tidy(expr), x_ptype = x, to_ptype = to)
  expect_identical(!!out, !!result)
}

expect_args <- function(x, y, x_arg, y_arg) {
  err <- catch_cnd(vec_type2(x, y, x_arg = x_arg, y_arg = y_arg), classes = "vctrs_error_incompatible_type")
  expect_true(!is_null(err))

  expect_true(grepl(paste0("for `", x_arg, "`"), err$message, fixed = TRUE))
  expect_true(grepl(paste0("and `", y_arg, "`"), err$message, fixed = TRUE))

  expect_identical(list(err$x_arg, err$y_arg), list(x_arg, y_arg))
}

try2 <- function(expr) {
  cat(paste0("\n", as_label(substitute(expr)), ":\n\n"))
  cat(catch_cnd(expr, classes = "error")$message, "\n\n")
}
