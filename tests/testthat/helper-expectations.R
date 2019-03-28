expect_dim <- function(x, shape) {
  dim <- vec_dim(x)
  expect_equal(dim, !!shape)
}

expect_lossy <- function(expr, result, ...) {
  expr <- enquo(expr)
  expect_error(eval_tidy(expr), class = "vctrs_error_cast_lossy")

  out <- suppress_errors_lossy_cast(eval_tidy(expr), ...)
  expect_identical(!!out, !!result)
}
