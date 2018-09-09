context("test-empty")

test_that("uses y when x is empty", {
  expect_equal(1 %0% 2, 1)
  expect_equal(1[0] %0% 2, 2)
})
