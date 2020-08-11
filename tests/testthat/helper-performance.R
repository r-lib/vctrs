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

# From r-lib/bench
with_memory_prof <- function(expr) {
  f <- tempfile()
  on.exit(unlink(f))

  utils::Rprofmem(f, threshold = 1)
  on.exit(utils::Rprofmem(NULL), add = TRUE)

  res <- force(expr)
  utils::Rprofmem(NULL)

  bytes <- parse_allocations(f)$bytes
  sum(bytes, na.rm = TRUE)
}
parse_allocations <- function(filename) {
  if (!is_installed("profmem")) {
    testthat::skip("profmem must be installed.")
  }
  readRprofmem <- env_get(ns_env("profmem"), "readRprofmem")

  tryCatch(
    readRprofmem(filename),
    error = function(cnd) {
      testthat::skip(sprintf(
        "Memory profiling failed: %s",
        conditionMessage(cnd)
      ))
    }
  )
}
