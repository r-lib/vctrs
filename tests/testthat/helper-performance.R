skip_if_not_testing_performance <- function(x) {
  opt <- Sys.getenv("VCTRS_TEST_PERFORMANCE", unset = "false")
  testing <- identical(opt, "true")

  if (testing) {
    return()
  }

  skip("Not testing performance")
}

expect_time_lt <- function(expr, expect) {
  time <- time_of({{ expr }})
  expect_lt(time, expect)
}

time_of <- function(expr) {
  expr <- enquo(expr)
  time <- system.time(eval_tidy(expr))
  unclass(time)[["elapsed"]]
}
