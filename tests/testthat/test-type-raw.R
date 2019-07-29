context("test-type-raw")

test_that("can sort raw", {
  x <- as.raw(c(3, 1, 2, 4))
  expect_identical(vec_order(x), int(2, 3, 1, 4))
  expect_identical(x[vec_order(x)], as.raw(1:4))
})
