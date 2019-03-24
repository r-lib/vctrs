context("test-conditions")

test_that("warn_lossy_cast", {
  expect_warning(
    warn_lossy_cast(integer(), character(), locations = 1:3),
    class = "warning_lossy_cast"
  )
  expect_warning(
    warn_lossy_cast(integer(), character(), locations = integer()),
    NA
  )
})
